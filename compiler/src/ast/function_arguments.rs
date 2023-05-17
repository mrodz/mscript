use crate::parser::{Parser, Node};

use super::Ident;

#[derive(Debug)]
pub struct FunctionArguments(Vec<Ident>);

impl Parser {
	pub fn function_arguments(input: Node) -> FunctionArguments {
		let children = input.children();

		let mut result = vec![];

		for child in children {
			result.push(Self::ident(child));
		}

		FunctionArguments(result)
	}
}