use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{function_body::FunctionBody, FunctionArguments};

#[derive(Debug)]
pub struct Function {
    pub arguments: FunctionArguments,
    pub body: FunctionBody,
}

impl Parser {
	pub fn function(input: Node) -> Result<Function> {
		let mut children = input.children();
		let arguments = children.next().unwrap();
		let function_body = children.next().unwrap();

		let arguments = Self::function_arguments(arguments);
		let body = Self::function_body(function_body);

		Ok(Function { arguments, body })
	}
}