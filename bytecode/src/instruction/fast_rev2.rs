use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, instruction_constants::FAST_REV2, serialization::SerializedInstructionBuilder};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct FastRev2;

impl Instruction for FastRev2 {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 2 {
			bail!("fast_rev2 requires a stack size of 2");
		}

		let Some([first, second]) = context.get_many_op_items_mut(0..2) else {
			bail!("could not get op items");
		};

		let first_cloned = first.clone();
		*first = second.clone();
		*second = first_cloned;

		Ok(InstructionExitState::Processed)
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(FAST_REV2).build()
	}

	fn deserialize(bytes: &[u8]) -> Self {
		Self
	}
}