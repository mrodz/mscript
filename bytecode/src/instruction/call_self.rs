use anyhow::{Result, Context};

use crate::{Ctx, Instruction, InstructionExitState, serialization::SerializedInstructionBuilder, instruction_constants::CALL_SELF};

use super::{JumpRequestDestination, JumpRequest};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct CallSelf;

impl Instruction for CallSelf {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let arguments = context.get_local_operating_stack();

        let callback_state = context.get_callback_variables();

        let stack = context.rced_call_stack();
        let name = stack
            .get_executing_function_label()
            .context("not run in a function")?;

		let exit = InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Standard(name.clone()),
            callback_state,
            stack,
            arguments,
        });

        context.clear_stack();

        Ok(exit)
    }

    fn deserialize(bytes: &[u8]) -> Self {
        Self
    }

    fn serialize(&self) -> Vec<u8> {
        SerializedInstructionBuilder::new(CALL_SELF)
            .build()
    }
}
