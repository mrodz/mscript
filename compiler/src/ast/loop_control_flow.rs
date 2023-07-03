use anyhow::Result;

use crate::parser::{Parser, Node};

use super::{CompiledItem, Compile};

#[derive(Debug)]
pub(crate) struct Continue;

impl Compile for Continue {
	fn compile(&self, _function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>, anyhow::Error> {
		const JMP_ID: u8 = 0x1D;
		let jmp = CompiledItem::Instruction { id: JMP_ID, arguments: Box::new([String::from("@@@@@@@@@@")])};

		Ok(vec![jmp])
	}
}

impl Parser {
	pub fn continue_statement(_input: Node) -> Result<Continue> {
		// TODO: check if is in loop
		Ok(Continue)
	}
}