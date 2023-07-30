use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{Compile, CompiledItem, Dependencies, Dependency, Value};

#[derive(Debug)]
pub struct PrintStatement(Value);

impl Dependencies for PrintStatement {
    fn dependencies(&self) -> Vec<Dependency> {
        self.0.net_dependencies()
    }
}

impl Compile for PrintStatement {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut matched = self.0.compile(function_buffer)?;
        matched.append(&mut vec![instruction!(printn "*"), instruction!(void)]);
        Ok(matched)
    }
}

impl Parser {
    pub fn print_statement(input: Node) -> Result<PrintStatement, Vec<anyhow::Error>> {
        let item = input.children().next().unwrap();

        let value = Self::value(item)?;

        Ok(PrintStatement(value))
    }
}
