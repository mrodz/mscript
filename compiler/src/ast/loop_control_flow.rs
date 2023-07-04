use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{CompiledItem, Compile};

#[derive(Debug)]
pub(crate) struct Continue;
#[derive(Debug)]
pub(crate) struct Break;

impl Compile for Continue {
	fn compile(&self, _function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>, anyhow::Error> {
		Ok(vec![CompiledItem::Continue])
	}
}

impl Compile for Break {
	fn compile(&self, _function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>, anyhow::Error> {
		Ok(vec![CompiledItem::Break])
	}
}

impl Parser {
	pub fn break_statement(_input: Node) -> Result<Break> {
		// TODO: check if is in loop
		Ok(Break)
	}

	pub fn continue_statement(_input: Node) -> Result<Continue> {
		// TODO: check if is in loop
		Ok(Continue)
	}
}