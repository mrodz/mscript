use std::sync::Arc;

use anyhow::Result;

use crate::{
    ast::{
        function::FunctionType, r#type::IntoType, Block, Dependencies, FunctionParameters,
        TypeLayout,
    },
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr,
};

#[derive(Debug)]
pub struct Constructor {
    parameters: FunctionParameters,
    body: Block,
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
