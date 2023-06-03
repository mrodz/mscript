use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{Compile, CompiledItem, Declaration, Dependencies, Dependency};

#[derive(Debug, Clone)]
pub struct Block(Vec<Declaration>);

impl Block {
    pub fn empty_body() -> Self {
        Self(vec![])
    }
}

impl Dependencies for Block {
    fn supplies(&self) -> Vec<Dependency> {
        self.0.iter().flat_map(|x| x.supplies()).collect()
    }

    fn dependencies(&self) -> Vec<Dependency> {
        self.0
            .iter()
            // .filter_map(|x| )
            .flat_map(|x| x.net_dependencies())
            .collect()
    }
}

impl Compile for Block {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        let compiled_body: Vec<super::CompiledItem> = self
            .0
            .iter()
            .flat_map(|x| x.compile(function_buffer).unwrap())
            // .flatten()
            .collect();

        Ok(compiled_body)
    }
}

impl Parser {
    pub fn block(input: Node) -> Result<Block> {
        let children = input.children();

        let mut result = vec![];

        for child in children {
            result.push(Self::declaration(child)?)
        }

        Ok(Block(result))
    }
}
