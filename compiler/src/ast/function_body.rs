use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{
    get_net_dependencies, CompilationState, Compile, Declaration, Dependencies, Dependency,
};

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
        let block_dependencies = self.0.iter().flat_map(|x| x.net_dependencies()).collect();

        dbg!(block_dependencies)
    }

    fn net_dependencies(&self) -> Vec<Dependency> {
        get_net_dependencies(self, true)
    }
}

impl Compile for Block {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>> {
        let compiled_body: Vec<super::CompiledItem> = self
            .0
            .iter()
            .flat_map(|x| x.compile(state).unwrap())
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

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(Block(result))
        }
    }
}
