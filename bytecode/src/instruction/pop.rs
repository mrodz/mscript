use anyhow::Result;

use crate::{InstructionExitState, Ctx, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Pop;

impl Instruction for Pop {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.pop();

		Ok(InstructionExitState::Processed)
	}
}