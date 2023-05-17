use anyhow::Result;

use crate::parser::{Parser, Node, Rule};

use super::{Function, Ident};

#[derive(Debug)]
pub enum Value {
    Function(Function),
    Ident(Ident),
}

impl Parser {
	pub fn value(input: Node) -> Result<Value> {
		let matched = match input.as_rule() {
			Rule::function => Value::Function(Self::function(input)?),
			Rule::ident => Value::Ident(Self::ident(input)),
			x => unreachable!("{x:?}")
		};

		Ok(matched)
	}
}