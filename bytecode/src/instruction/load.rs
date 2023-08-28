use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Load {
	name: Box<str>,
}

impl Instruction for Load {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(var) = context.load_variable(&self.name) else {
			bail!("load before store (`{}` not in scope)", self.name)
		};

		context.push(var.0.clone());

		Ok(InstructionExitState::Processed)
	}
}