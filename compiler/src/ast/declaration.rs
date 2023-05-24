use anyhow::Result;

use crate::parser::{Parser, Node, Rule};

use super::{assignment::Assignment, Dependencies, Callable, PrintStatement, Compile, Dependency};

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
	Assignment(Assignment),
	Callable(Callable),
	PrintStatement(PrintStatement)
}

impl Dependencies for Declaration {
	fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
		match self {
			Self::Assignment(assignment) => assignment.get_dependencies(),
			Self::Callable(callable) => callable.get_dependencies(),
			Self::PrintStatement(print_statement) => print_statement.get_dependencies(),
		}
	}
}

impl Compile for Declaration {
	fn compile(&self) -> Result<Vec<super::CompiledItem>> {
		match self {
			Self::PrintStatement(x) => x.compile(),
			Self::Callable(x) => x.compile(),
			Self::Assignment(x) => x.compile(),
		}
	}
}

impl Parser {
	pub fn declaration(input: Node) -> Result<Declaration> {
		let declaration = input.children().next().unwrap();

		let matched = match declaration.as_rule() {
			Rule::assignment => Declaration::Assignment(Self::assignment(declaration)?),
			Rule::callable => Declaration::Callable(Self::callable(declaration)?),
			Rule::print_statement => Declaration::PrintStatement(Self::print_statement(declaration)?),
			_ => unreachable!()
		};

		Ok(matched)

		// Ok(result)
	}
}