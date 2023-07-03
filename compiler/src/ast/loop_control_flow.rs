use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{CompiledItem, Compile};

#[derive(Debug)]
pub(crate) struct Continue {
	// parent_loop: Vec<&'a mut CompiledItem>
}

impl Compile for Continue {
	fn compile(&self, _function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>, anyhow::Error> {
		todo!()
	}
}

impl Parser {
	pub fn continue_statement(_input: Node) -> Result<Continue> {
		

		todo!()
	}
}