use std::slice::Iter;

use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{Dependencies, Dependency, Value};

#[derive(Debug, Clone)]
pub(crate) struct FunctionArguments(Vec<Value>);

impl FunctionArguments {
    pub fn iter(&self) -> Iter<Value> {
        self.0.iter()
    }
}

impl Dependencies for FunctionArguments {
    fn dependencies(&self) -> Vec<Dependency> {
        self.0.iter().flat_map(|x| x.net_dependencies()).collect()
    }
}

impl Parser {
    pub fn function_arguments(input: Node) -> Result<FunctionArguments> {
        let children = input.children();

        let mut result = vec![];

        for child in children {
            result.push(Self::value(child)?)
        }

        Ok(FunctionArguments(result))
    }
}
