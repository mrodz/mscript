use std::slice::Iter;

use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{Value, Dependencies, Dependency};

#[derive(Debug, Clone)]
pub(crate) struct FunctionArguments(Vec<Value>);

impl FunctionArguments {
	pub fn iter(&self) -> Iter<Value> {
		self.0.iter()
	}
}

impl Dependencies for FunctionArguments {
	fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
		let x: Vec<Dependency> = self.0.iter()
			.filter_map(|x| x.get_dependencies())
			.flat_map(|x| x.into_vec())
			.collect();

		if x.is_empty() {
			None
		} else {
			Some(x.into_boxed_slice())
		}
	}
}

impl Parser {
	pub fn function_arguments(input: Node) -> Result<FunctionArguments> {
		let children = input.children();

		let mut result = vec![];

		for child in children {
			result.push(Self::value(child)?)
		}

		Ok(FunctionArguments(result))
	}
}