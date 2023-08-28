use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct StoreObject {
	name: Box<str>,
}

impl Instruction for StoreObject {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 1 {
			bail!("store can only store a single item");
		}

		let arg = context.pop().unwrap().move_out_of_heap_primitive();

		context.update_callback_variable(self.name.as_ref(), arg)?;

		Ok(InstructionExitState::Processed)
	}
}