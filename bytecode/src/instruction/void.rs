use crate::function::InstructionExitState;

use super::Instruction;

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Void;

impl Instruction for Void {
	fn execute(&self, context: &mut crate::context::Ctx) -> anyhow::Result<crate::function::InstructionExitState> {
		context.clear_stack();
		Ok(InstructionExitState::Processed)
	}
}