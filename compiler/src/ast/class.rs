mod class_body;
mod class_feature;
mod constructor;
mod member_function;
mod member_variable;

use std::{borrow::Cow, fmt::Display, sync::Arc};

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
    VecErr,
};

use super::{
    function::FunctionType, r#type::IntoType, Compile, CompiledFunctionId, CompiledItem,
    Dependencies, Dependency, FunctionParameters, Ident,
};

pub(in crate::ast::class) trait WalkForType {
    fn type_from_node(input: &Node) -> Result<Ident>;
}

#[derive(Debug)]
pub struct Class {
    ident: Ident,
    body: ClassBody,
    path_str: Arc<String>,
}

impl Compile for Class {
    fn compile(&self, state: &super::CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let body_compiled = self.body.compile(state)?;

        let TypeLayout::Class(ty) = self.ident.ty()?.as_ref() else {
            unreachable!()
        };

        let id = CompiledFunctionId::Custom(format!("{}_{}", ty.name(), ty.id));

        let compiled_class = CompiledItem::Function {
            id: id.clone(),
            content: Some(body_compiled),
            location: Arc::clone(&self.path_str),
        };

        state.push_function(compiled_class);

        let name = self.ident.name();

        Ok(vec![
            instruction!(make_function(format!("{}#{id}", self.path_str))),
            instruction!(store_fast name),
        ])
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ClassType {
    name: Arc<String>,
    fields: Arc<[Ident]>,
    path_str: Arc<String>,
    id: usize,
}

impl ClassType {
    pub(crate) fn abs_id(&self) -> String {
        format!("{}_{}", self.name, self.id)
    }

    pub(crate) fn abs_class_path(&self) -> String {
        format!("{}#", self.path_str)
    }

    pub(crate) fn initialization_callable_path(&self) -> String {
        format!("{}#{}_{}", self.path_str, self.name, self.id)
    }

    pub fn constructor(&self) -> FunctionType {
        for field in self.fields() {
            if field.name() == "$constructor" {
                let x = field.clone().ty_owned().unwrap();
                return x.into_owned().owned_is_function().unwrap();
            }
        }

        // use default constructor if a class doesn't have one.

        let return_type = ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Class(self.clone())));
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

        let mut ident = Self::ident(ident_node).to_err_vec()?;

        let body_node = children.next().unwrap();

        let body = {
            let _class_scope = input.user_data().push_class_unknown_self();

            let fields = ClassBody::get_members(&body_node).to_err_vec()?;

            let class_type = ClassType {
                fields,
                path_str: input.user_data().get_file_name(),
                name: Arc::new(ident.name().to_owned()),
                id: input.user_data().request_class_id(),
            };

            ident
                .link_force_no_inherit(
                    input.user_data(),
                    Cow::Owned(TypeLayout::Class(class_type.clone())),
                )
                .to_err_vec()?;

            log::trace!("+class {}", ident.name());

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
            path_str: input.user_data().get_file_name(),
        };

        Ok(result)
    }
}
