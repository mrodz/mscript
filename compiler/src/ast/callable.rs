use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{Ident, FunctionArguments, Dependencies, };

#[derive(Debug)]
pub struct Callable {
	ident: Ident,
	function_arguments: FunctionArguments
}

impl Dependencies for Callable {
	fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
		// a call needs to have access to the function/object
		let maybe_arg_dependencies = self.function_arguments.get_dependencies();

		if let Some(arg_dependencies) = maybe_arg_dependencies {
			let mut result = Vec::with_capacity(arg_dependencies.len() + 1);
			
			result.push(&self.ident);
			result[1..].copy_from_slice(&arg_dependencies);

			Some(result.into_boxed_slice())


			// Some(.copy_from_slice(arg_dependencies))
		} else {
			Some(Box::new([&self.ident]))	
		}
	}
}

impl Parser {
	pub fn callable(input: Node) -> Result<Callable> {
		let mut children = input.children();

		let ident = children.next().unwrap();
		let ident = Self::ident(ident);

		let function_arguments = children.next().unwrap();
		let function_arguments = Self::function_arguments(function_arguments)?;

		Ok(Callable { ident, function_arguments })
	}
}