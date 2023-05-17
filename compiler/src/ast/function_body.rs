use crate::parser::{Node, Parser};

use super::statement::Statement;

#[derive(Debug)]
pub struct FunctionBody(Vec<Statement>);

impl Parser {
	pub fn function_body(input: Node) -> FunctionBody {
		let children = input.children();

		let mut result = vec![];

		for child in children {
			result.push(Self::statement(child))
		}

		FunctionBody(result)
	}
}