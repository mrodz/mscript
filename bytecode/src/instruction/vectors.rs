use anyhow::Result;

use crate::{Ctx, InstructionExitState, Instruction, vector};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct MakeVector {
    capacity: Option<usize>,
}

impl Instruction for MakeVector {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(capacity) = self.capacity else {
			let vec = context.clear_and_get_stack();
			// ^^ new capacity = old length
			context.push(vector!(raw vec));

			return Ok(InstructionExitState::Processed)
		};

		let vec = vector!(raw Vec::with_capacity(capacity));

        context.push(vec);

		Ok(InstructionExitState::Processed)
	}
}