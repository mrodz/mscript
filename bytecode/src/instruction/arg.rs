use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, instruction_constants::ARG_BYTE, SerializedInstructionBuilder, serialization::InstructionDeserializationFactory};

#[derive(Debug)]
pub struct Arg {
	index: usize,
}

impl Instruction for Arg {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(nth_arg) = context.nth_arg(self.index) else {
			bail!("#{} argument does not exist (range 0..{})", self.index, context.argc())
		};

		context.push(nth_arg.clone());

		Ok(InstructionExitState::Processed)
	}

	fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);
		Self {
			index: factory.next()
		}
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(ARG_BYTE)
			.add(self.index)
			.build()
	}
}