mod class_body;
mod class_feature;
mod constructor;
mod member_function;
mod member_variable;

use std::{borrow::Cow, fmt::Display, sync::Arc, path::PathBuf};

use anyhow::Result;

pub(crate) use class_body::ClassBody;
pub(crate) use constructor::Constructor;
pub(crate) use member_function::MemberFunction;
pub(crate) use member_variable::MemberVariable;

use crate::{
    ast::TypeLayout,
    instruction,
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr, BytecodePathStr,
};

use super::{
    function::FunctionType, new_err, r#type::IntoType, Compile, CompiledFunctionId, CompiledItem,
    Dependencies, Dependency, FunctionParameters, Ident,
};

pub(in crate::ast::class) trait WalkForType {
    fn type_from_node(input: &Node) -> Result<Ident>;
}

#[derive(Debug)]
pub struct Class {
    ident: Ident,
    body: ClassBody,
    path_str: Arc<PathBuf>,
}

impl Compile for Class {
    fn compile(&self, state: &super::CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let body_compiled = self.body.compile(state)?;

        let TypeLayout::Class(ty) = self.ident.ty()?.as_ref() else {
            unreachable!()
        };

        let id = CompiledFunctionId::Custom(ty.name().to_owned());

        let compiled_class = CompiledItem::Function {
            id: id.clone(),
            content: Some(body_compiled),
            location: Arc::clone(&self.path_str),
        };

        state.push_function(compiled_class);

        let name = self.ident.name();

        let function_name = format!("{}#{id}", self.path_str.bytecode_str());

        Ok(vec![
            instruction!(make_function function_name),
            instruction!(export_special name id),
        ])
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ClassType {
    name: Arc<String>,
    fields: Arc<[Ident]>,
    path_str: Arc<PathBuf>,
    id: usize,
}

impl ClassType {
    pub fn constructor(&self) -> FunctionType {
        let return_type = ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Class(self.clone())));

        for field in self.fields() {
            if field.name() == "$constructor" {
                let x = field.clone().ty_owned().unwrap();
                if let Some(mut constructor) = x.into_owned().owned_is_function() {
                    constructor.set_return_type(return_type);
                    return constructor;
                }
                unreachable!()
            }
        }

        // use default constructor if a class doesn't have one.

        let empty_parameters = Arc::new(FunctionParameters::TypesOnly(vec![]));

        FunctionType::new(empty_parameters, return_type)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn arced_name(&self) -> Arc<String> {
        Arc::clone(&self.name)
    }

    pub fn fields(&self) -> &[Ident] {
        &self.fields
    }

    pub(crate) fn arced_fields(&self) -> Arc<[Ident]> {
        Arc::clone(&self.fields)
    }

    pub fn get_property<'a>(&'a self, name: &str) -> Option<&'a Ident> {
        self.fields.iter().find(|&field| field.name() == name)
    }
}

impl Display for ClassType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "class {} {{ ", self.name())?;

        let mut fields = self.fields.iter();

        if let Some(field) = fields.next() {
            write!(f, "{field}")?;
        }

        for field in fields {
            write!(f, ", {field}")?;
        }

        write!(f, " }}")
    }
}

impl IntoType for Class {
    fn for_type(&self) -> Result<super::TypeLayout> {
        Ok(self.ident.ty()?.clone().into_owned())
    }
}

impl Dependencies for Class {
    fn dependencies(&self) -> Vec<Dependency> {
        self.body.net_dependencies()
    }

    fn supplies(&self) -> Vec<Dependency> {
        vec![Dependency::new(Cow::Borrowed(&self.ident))]
    }
}

impl Parser {
    pub fn class(input: Node) -> Result<Class, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident_node = children.next().unwrap();
        let ident_span = ident_node.as_span();

        let mut ident = Self::ident(ident_node).to_err_vec()?;
        ident.mark_const();

        let has_been_declared = input.user_data().get_ident_from_name_local(ident.name());

        if let Some(has_been_declared) = has_been_declared {
            return Err(vec![new_err(ident_span, &input.user_data().get_source_file_name(), format!("This name is already in scope (Hint: `{}: {} = ...` was declared somewhere above)", ident.name(), has_been_declared.ty().unwrap()))]);
        }

        let body_node = children.next().unwrap();

        let body = {
            let _class_scope = input.user_data().push_class_unknown_self();

            let fields = ClassBody::get_members(&body_node).to_err_vec()?;

            let class_type = ClassType {
                fields,
                path_str: input.user_data().bytecode_path(),
                name: Arc::new(ident.name().to_owned()),
                id: input.user_data().request_class_id(),
            };

            ident
                .link_force_no_inherit(
                    input.user_data(),
                    Cow::Owned(TypeLayout::Class(class_type.clone())),
                )
                .to_err_vec()?;

            log::trace!("class {} {{ ... }}", ident.name());

            input.user_data().set_self_type_of_class(class_type);

            Self::class_body(body_node)?
        };

        input.user_data().add_type(
            ident.name().clone().into_boxed_str(),
            ident.ty().unwrap().clone().into_owned(),
        );

        input.user_data().add_dependency(&ident);

        let result = Class {
            ident,
            body,
            path_str: input.user_data().bytecode_path(),
        };

        Ok(result)
    }
}
