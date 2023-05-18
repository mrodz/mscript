use anyhow::Result;

use crate::parser::{Parser, Node, Rule};

use super::{assignment::Assignment, Dependencies, Callable};

#[derive(Debug)]
pub enum Declaration {
	Assignment(Assignment),
	Callable(Callable),
}

impl Dependencies for Declaration {
	fn get_dependencies(&self) -> Option<Box<[&super::Ident]>> {
		match self {
			Self::Assignment(assignment) => {
				assignment.get_dependencies()
			}
			Self::Callable(callable) => {
				callable.get_dependencies()
			}
		}
	}
}

impl Parser {
	pub fn declaration(input: Node) -> Result<Declaration> {
		let declaration = input.children().next().unwrap();

		let matched = match declaration.as_rule() {
			Rule::assignment => Declaration::Assignment(Self::assignment(declaration)?),
			Rule::callable => Declaration::Callable(Self::callable(declaration)?),
			_ => unreachable!()
		};

		Ok(matched)

		// Ok(result)
	}
}