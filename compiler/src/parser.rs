use std::{fs::File, path::Path, io::{BufReader, Read}};
use anyhow::{Result, bail};
use pest_consume::Parser as ParserDerive;

#[allow(unused)]
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn root_node_from_str(str: &str) -> Result<Node> {
	Ok(<Parser as pest_consume::Parser>::parse(Rule::file, str)?.single()?)
}

#[pest_consume::parser]
impl Parser {
	pub fn top_level_declarations(input: Node) -> Result<Vec<String>> {
		let Rule::top_level_declarations = input.as_rule() else {
			bail!("not top_level_declarations")
		};

		dbg!(input.children());

		Ok(vec![])
	}

	pub fn file(input: Node) -> Result<Vec<String>> {
		for child in input.children() {
			match child.as_rule() {
				top_level_declarations @ Rule::top_level_declarations => {
					Self::top_level_declarations(child)?;
					dbg!(top_level_declarations);
				}
				other => {
					dbg!(other);
				}
			}
		}

		Ok(vec![])
	}
}

