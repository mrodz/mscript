use std::slice::Iter;

use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{Dependencies, Dependency, value::ValueChain};

#[derive(Debug)]
pub(crate) struct FunctionArguments(Vec<ValueChain>);

impl FunctionArguments {
    pub fn iter(&self) -> Iter<ValueChain> {
        self.0.iter()
    }
}

impl Dependencies for FunctionArguments {
    fn dependencies(&self) -> Vec<Dependency> {
        self.0.iter().flat_map(|x| x.net_dependencies()).collect()
    }
}

impl Parser {
    pub fn function_arguments(input: Node) -> Result<FunctionArguments, Vec<anyhow::Error>> {
        let children = input.children();

        let mut result = vec![];

        for child in children {
            result.push(Self::value(child)?)
        }

        Ok(FunctionArguments(result))
    }
}
