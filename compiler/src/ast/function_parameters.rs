use crate::parser::{Parser, Node};

use super::Ident;

#[derive(Debug)]
pub struct FunctionParameters(Vec<Ident>);

impl Parser {
	pub fn function_parameters(input: Node) -> FunctionParameters {
		let children = input.children();

		let mut result = vec![];

		for child in children {
			result.push(Self::ident(child));
		}

		FunctionParameters(result)
	}
}