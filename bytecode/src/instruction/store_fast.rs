use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct StoreFast {
	name: Box<str>,
}

impl Instruction for StoreFast {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 1 {
			bail!("store can only store a single item");
		}

		let arg = context.pop().unwrap().move_out_of_heap_primitive();

		context.register_variable_local(self.name.to_string(), arg)?;

		Ok(InstructionExitState::Processed)
	}
}