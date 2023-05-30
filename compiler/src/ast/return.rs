use anyhow::Result;

use crate::{parser::{Parser, Node}, instruction};

use super::{Value, Dependencies, Compile};

#[derive(Debug, Clone)]
pub(crate) struct ReturnStatement(Value);

impl Compile for ReturnStatement {
	fn compile(&self, function_buffer: &mut Vec<super::CompiledItem>) -> Result<Vec<super::CompiledItem>> {
		let mut value_init = self.0.compile(function_buffer)?;
		value_init.push(instruction!(ret));
		Ok(value_init)
	}
}

impl Dependencies for ReturnStatement {
	fn supplies(&self) -> Vec<super::Dependency> {
		vec![]
	}

	fn dependencies(&self) -> Vec<super::Dependency> {
		self.0.net_dependencies()
	}
}

impl Parser {
	pub fn return_statement(input: Node) -> Result<ReturnStatement> {
		let value_node = input.children().next().unwrap();

		let value = Self::value(value_node)?;

		Ok(ReturnStatement(value))
	}
}