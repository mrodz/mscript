use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use anyhow::{bail, Result};

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{
    new_err, r#type::TypeLayout, CompilationState, Compile, CompiledItem, Dependencies, Dependency,
    Ident,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum FunctionParameters {
    Named(Vec<Ident>),
    TypesOnly(Vec<Cow<'static, TypeLayout>>),
}

impl FunctionParameters {
    pub fn len(&self) -> usize {
        match self {
            Self::Named(x) => x.len(),
            Self::TypesOnly(x) => x.len(),
        }
    }

    pub fn to_types(&self) -> Cow<'_, Vec<Cow<'static, TypeLayout>>> {
        match self {
            FunctionParameters::Named(names) => {
                Cow::Owned(names.iter().map(|x| x.ty().unwrap().clone()).collect())
            }
            FunctionParameters::TypesOnly(types) => Cow::Borrowed(types),
        }
    }
}

impl Display for FunctionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf: String = String::new();

        let types: Cow<Vec<Cow<TypeLayout>>> = self.to_types();

        let mut iter = types.iter();
        let Some(first) = iter.next() else {
            return Ok(());
        };

        buf.push_str(&first.to_string());

        for param in iter {
            buf.push_str(", ");
            buf.push_str(&param.to_string());
        }

        write!(f, "{buf}")
    }
}

impl Dependencies for FunctionParameters {
    fn supplies(&self) -> Vec<Dependency> {
        let Self::Named(names) = self else {
            unreachable!("cannot compile nameless function parameters");
        };

        names
            .iter()
            .map(|x| Dependency::new(Cow::Borrowed(x)))
            .collect()
    }
}

impl Compile for FunctionParameters {
    fn compile(&self, _: &CompilationState) -> Result<Vec<CompiledItem>> {
        let Self::Named(names) = self else {
            bail!("cannot compile unnamed function parameters (typically found in a type declaration) for a normal function")
        };

        let mut result = vec![];
        for (idx, ident) in names.iter().enumerate() {
            let name = ident.name();
            result.push(instruction!(arg idx));
            result.push(instruction!(store name));
        }
        Ok(result)
    }
}

impl Parser {
    /// this is the first thing that gets parsed in a function
    pub fn function_parameters(
        input: Node,
        add_to_scope_dependencies: bool,
        require_self_param: bool,
        allow_self_type: bool,
    ) -> Result<FunctionParameters> {
        let mut children = input.children();

        let mut result: Vec<Ident> = vec![];

        let file_name = input.user_data().get_source_file_name();

        let mut satifies_self_param = !require_self_param;

        for c in 0.. {
            let Some(ident_node) = children.next() else {
                break;
            };

            if c == 0 {
                let ident_str = ident_node.as_str();
                if ident_str == "self" && input.user_data().is_function_a_class_method() {
                    if add_to_scope_dependencies {
                        let ident = Ident::new(
                            "self".to_owned(),
                            Some(Cow::Owned(TypeLayout::ClassSelf(None))),
                            false,
                        );
                        input.user_data().add_dependency(&ident);
                        result.push(ident);
                    }

                    if require_self_param {
                        satifies_self_param = true;
                    }

                    continue;
                }
            }

            let ident_span = ident_node.as_span();
            let mut ident = Self::ident(ident_node)?;

            let ty: Option<Node> = children.next();

            let err = || {
                bail!(new_err(ident_span, &file_name, format!("for type safety, function parameters require type signatures. (try: `{}: type`)", ident.name())))
            };

            let Some(ty) = ty else { err()? };

            if ty.as_rule() != Rule::r#type {
                err()?
            }

            let ty_span = ty.as_span();

            let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;

            if !allow_self_type && ty.is_class_self() {
                return Err(new_err(ty_span, &input.user_data().get_source_file_name(), "`Self` is only a valid type for associated functions, and not normal functions. (Hint: if trying to accept a callback function, use a function type like `fn(int) -> bool`)".to_owned()));
            }

            ident.link_force_no_inherit(input.user_data(), ty)?;

            result.push(ident);
        }

        if !satifies_self_param {
            return Err(new_err(input.as_span(), &input.user_data().get_source_file_name(), "this function must take `self` as the first parameter because it is defined as a class instance fn".to_owned()));
        }

        Ok(FunctionParameters::Named(result))
    }
}
