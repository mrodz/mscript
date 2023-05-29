use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{Compile, CompiledItem, Declaration, Dependencies, Dependency};

#[derive(Debug, Clone)]
pub struct FunctionBody(Vec<Declaration>);

impl FunctionBody {
    pub fn empty_body() -> Self {
        Self(vec![])
    }
}

impl Dependencies for FunctionBody {
    fn supplies(&self) -> Vec<Dependency> {
        self
            .0
            .iter()
            .flat_map(|x| x.supplies())
            .collect()
    }

    fn dependencies(&self) -> Vec<Dependency> {
        self
            .0
            .iter()
            // .filter_map(|x| )
            .flat_map(|x| x.net_dependencies())
            .collect()
    }
}

impl Compile for FunctionBody {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        let mut compiled_body: Vec<super::CompiledItem> = self
            .0
            .iter()
            .flat_map(|x| x.compile(function_buffer).unwrap())
            // .flatten()
            .collect();

        const RET: u8 = 0x12;

        if let Some(CompiledItem::Instruction { id: RET, .. }) = compiled_body.last() {
            // nothing! the function returns by itself
        } else {
            compiled_body.push(instruction!(void));
            compiled_body.push(instruction!(ret));
        }

        Ok(compiled_body)
    }
}

impl Parser {
    pub fn function_body(input: Node) -> Result<FunctionBody> {
        let children = input.children();

        let mut result = vec![];

        for child in children {
            result.push(Self::declaration(child)?)
        }

        Ok(FunctionBody(result))
    }
}
