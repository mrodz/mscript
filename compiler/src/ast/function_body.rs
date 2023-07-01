use anyhow::Result;

use crate::{parser::{Node, Parser}};

use super::{Compile, CompiledItem, Declaration, Dependencies, Dependency, get_net_dependencies};

#[derive(Debug)]
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
        let block_dependencies = self.0
            .iter()
            .flat_map(|x| x.net_dependencies())
            .collect();

        block_dependencies
    }

    fn net_dependencies(&self) -> Vec<Dependency> {
        get_net_dependencies(self, true)
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
    pub fn block(input: Node) -> Result<Block, Vec<anyhow::Error>> {
        let children = input.children();

        let mut result = vec![];

        let mut errors = vec![];

        for child in children {
            match Self::declaration(child) {
                Ok(declaration) => result.push(declaration),
                Err(mut e) => errors.append(&mut e),
            }
        }

        if errors.len() != 0 {
            Err(errors)
        } else {
            Ok(Block(result))
        }
    }
}
