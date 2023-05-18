use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{FunctionParameters, Dependencies, FunctionBody};

#[derive(Debug)]
pub struct Function {
    pub arguments: FunctionParameters,
    pub body: FunctionBody,
}

impl Dependencies for Function {
	fn get_dependencies(&self) -> Option<Box<[&super::Ident]>> {
		self.body.get_dependencies()
	}
}

impl Parser {
	pub fn function(input: Node) -> Result<Function> {
		let mut children = input.children();
		let arguments = children.next().unwrap();
		let function_body = children.next().unwrap();

		let arguments = Self::function_parameters(arguments);
		let body = Self::function_body(function_body)?;

		Ok(Function { arguments, body })
	}
}