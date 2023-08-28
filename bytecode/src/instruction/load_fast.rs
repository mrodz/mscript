use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct LoadFast {
	name: Box<str>,
}

impl Instruction for LoadFast {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(var) = context.load_local(&self.name) else {
			bail!("load_fast before store (`{}` not in this stack frame)\nframe `{}`'s variables:\n{}", self.name, context.rced_call_stack().get_frame_label(), context.get_frame_variables())
		};

		context.push(var.0.clone());

		Ok(InstructionExitState::Processed)
	}
}