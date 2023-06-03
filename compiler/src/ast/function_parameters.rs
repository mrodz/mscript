use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use anyhow::{bail, Result};

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{new_err, r#type::TypeLayout, Compile, CompiledItem, Dependencies, Dependency, Ident};

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn to_types(&self) -> Cow<Vec<Cow<TypeLayout>>> {
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
            return Ok(())
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
            return vec![];
        };

        names
            .iter()
            .map(|x| Dependency::new(Cow::Borrowed(x)))
            .collect()
    }
}

impl Compile for FunctionParameters {
    fn compile(&self, _: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
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
    pub fn function_parameters(input: Node) -> Result<FunctionParameters> {
        let mut children = input.children();

        let mut result: Vec<Ident> = vec![];

        let file_name = input.user_data().get_source_file_name();

        loop {
            let Some(ident_node) = children.next() else {
                break;
            };

            let ident_span = ident_node.as_span();
            let mut ident = Self::ident(ident_node)?;

            let ty: Option<Node> = children.next();

            let err = || {
                bail!(new_err(ident_span, &file_name, format!("for type safety, function parameters require type signatures. (try: `{}: type`)", ident.name())))
            };

            let Some(ty) = ty else {
                err()?
            };

            if ty.as_rule() != Rule::r#type {
                err()?
            }

            let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;

            ident.link(input.user_data(), Some(ty))?;

            result.push(ident);
        }

        Ok(FunctionParameters::Named(result))
    }
}
