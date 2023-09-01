use std::{sync::Arc, borrow::Cow};

use anyhow::Result;

use crate::{
    ast::{
        function::FunctionType, r#type::IntoType, Block, Dependencies, FunctionParameters,
        TypeLayout, Ident,
    },
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::WalkForType;

#[derive(Debug)]
pub struct Constructor {
    parameters: FunctionParameters,
    body: Block,
}

impl WalkForType for Constructor {
    fn type_from_node(input: &Node) -> Result<Ident> {
        let parameters = input.children().next().unwrap();
        let parameters = Parser::function_parameters(parameters, false)?;

        let function_type = FunctionType::new(Arc::new(parameters), ScopeReturnStatus::Void);

        let ident = Ident::new("$constructor".to_owned(), Some(Cow::Owned(TypeLayout::Function(function_type))), true);

        Ok(ident)
    }
}

impl IntoType for Constructor {
    fn for_type(&self) -> Result<crate::ast::TypeLayout> {
        let function_type =
            FunctionType::new(Arc::new(self.parameters.clone()), ScopeReturnStatus::Void);

        Ok(TypeLayout::Function(function_type))
    }
}

impl Dependencies for Constructor {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        self.body.net_dependencies()
    }

    fn supplies(&self) -> Vec<crate::ast::Dependency> {
        self.parameters.supplies()
    }
}

impl Parser {
    pub fn constructor(input: Node) -> Result<Constructor, Vec<anyhow::Error>> {
        let mut children = input.children();
        let parameters = children.next().unwrap();
        let parameters = Self::function_parameters(parameters, true).to_err_vec()?;

        let body = children.next().unwrap();
        let body = Self::block(body)?;

        Ok(Constructor { parameters, body })
    }
}
