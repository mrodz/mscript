use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, HeapPrimitive};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct VecMut;

impl Instruction for VecMut {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() < 2 {
			bail!("mutating a vec requires [ptr, value]");
		}

		let new_val = context.pop().unwrap();
		let maybe_vec = context.pop().unwrap();

		let HeapPrimitive(vec_ptr) = maybe_vec else {
			bail!("expected a mutable heap primitive, found {maybe_vec}");
		};

		unsafe {
			*vec_ptr = new_val;
		}

		Ok(InstructionExitState::Processed)
	}
}