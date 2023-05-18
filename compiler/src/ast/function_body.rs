use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{Dependencies, Ident, Declaration};

#[derive(Debug)]
pub struct FunctionBody(Vec<Declaration>);

impl Dependencies for FunctionBody {
	fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
		let x: Vec<&Ident> = self.0.iter()
			.filter_map(|x| x.get_dependencies())
			.flat_map(|x| x.into_vec())
			.collect();

		let x = x.into_boxed_slice();

		if x.is_empty() {
			None
		} else {
			Some(x)
		}

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