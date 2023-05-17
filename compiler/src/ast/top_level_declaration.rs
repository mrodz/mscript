use anyhow::Result;

use crate::parser::{Parser, Node, Rule};

use super::assignment::Assignment;

#[derive(Debug)]
pub enum TopLevelDeclaration {
	Assignment(Assignment)
}

impl Parser {
	pub fn top_level_declaration(input: Node) -> Result<TopLevelDeclaration> {
		let declaration = input.children().next().unwrap();

		let matched = match declaration.as_rule() {
			Rule::assignment => TopLevelDeclaration::Assignment(Self::assignment(declaration)?),
			_ => unreachable!()
		};

		Ok(matched)

		// Ok(result)
	}
}