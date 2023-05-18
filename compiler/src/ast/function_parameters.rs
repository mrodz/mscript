use crate::{parser::{Parser, Node}, instruction};

use super::{Ident, Dependencies, Compile};

#[derive(Debug)]
pub struct FunctionParameters(Vec<Ident>);

impl Dependencies for FunctionParameters {}

impl Compile for FunctionParameters {
	fn compile(&self) -> Vec<super::CompiledItem> {
		let mut result = vec![];
		for (idx, ident) in self.0.iter().enumerate() {
			result.push(instruction!(arg idx));
			result.push(instruction!(store ident));
		}
		result
	}
}

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