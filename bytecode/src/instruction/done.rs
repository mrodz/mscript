use anyhow::Result;

use crate::{Ctx, Instruction, InstructionExitState, serialization::SerializedInstructionBuilder, instruction_constants::DONE};

use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Done;

impl Instruction for Done {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        Ok(InstructionExitState::PopScope)
    }

    fn deserialize(bytes: &[u8]) -> Self {
		Self
	}

    fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(DONE).build()
	}
}
