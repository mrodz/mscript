use std::{borrow::Cow, fmt::Display};

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{r#type::TypeLayout, Compile, Dependencies, Dependency, Ident};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParameters(Vec<Ident>);

impl Display for FunctionParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = format!("{:?}", self.0);
        write!(f, "{}", &x[1..x.len() - 1])
    }
}

impl Dependencies for FunctionParameters {
    fn supplies(&self) -> Option<Box<[super::Dependency]>> {
        let mapped = self
            .0
            .iter()
            .map(|x| Dependency::new(Cow::Borrowed(x)))
            .collect();

        Some(mapped)
    }
}

impl Compile for FunctionParameters {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let mut result = vec![];
        for (idx, ident) in self.0.iter().enumerate() {
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

        let mut result = vec![];

        while let (Some(ident), Some(ty)) = (children.next(), children.next()) {
            let mut ident = Self::ident(ident);
            let ty: &'static TypeLayout = Self::r#type(ty)?;

            // ident.link(input.user_data(), Some(Cow::Owned(TypeLayout::CallbackVariable(ty))))?;
            ident.link(input.user_data(), Some(Cow::Borrowed(ty)))?;

            result.push(ident);
        }

        Ok(FunctionParameters(result))
    }
}
