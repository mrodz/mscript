use anyhow::Result;

use crate::parser::{Parser, Node};

use super::TypeLayout;

impl Parser {
	pub fn function_return_type(input: Node) -> Result<&'static TypeLayout> {
		let ty = input.children().next().unwrap();
		Self::r#type(ty)
	}
}