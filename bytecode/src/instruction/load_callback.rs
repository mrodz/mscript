use anyhow::Result;

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct LoadCallback {
	name: Box<str>,
}

impl Instruction for LoadCallback {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let var = context.load_callback_variable(&self.name)?;
		context.push(var.0.clone());
		Ok(InstructionExitState::Processed)		
	}
}