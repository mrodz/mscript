use anyhow::{Result};
use pest_consume::{Parser as ParserDerive};

use crate::ast::TopLevelDeclaration;

#[allow(unused)]
pub type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn root_node_from_str(str: &str) -> Result<Node> {
	Ok(<Parser as pest_consume::Parser>::parse(Rule::file, str)?.single()?)
}

#[pest_consume::parser]
impl Parser {
	pub fn file(input: Node) -> Result<Vec<TopLevelDeclaration>> {
		let mut result = vec![];
		for child in input.children() {
			match child.as_rule() {
				Rule::top_level_declaration => {
					result.push(Self::top_level_declaration(child)?);
				}
				_ => ()
			}
		}

		Ok(result)
	}
}

