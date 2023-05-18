use anyhow::{Result};
use pest_consume::{Parser as ParserDerive};

use crate::ast::Declaration;

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
	pub fn file(input: Node) -> Result<Vec<Declaration>> {
		let mut result = vec![];
		for child in input.children() {
			match child.as_rule() {
				Rule::declaration => {
					result.push(Self::declaration(child)?);
				}
				_ => ()
			}
		}

		Ok(result)
	}
}

