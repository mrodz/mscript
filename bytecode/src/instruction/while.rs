use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, Bool, context::SpecialScope};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct While {
	offset: isize,
}

impl Instruction for While {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() == 0 {
			bail!("while statements require at least one entry in the local stack")
		}

		let item = context.pop().unwrap();
		context.clear_stack();

		let Bool(b) = item else {
			bail!("while statement can only test booleans")
		};

		Ok(if b {
			InstructionExitState::PushScope(SpecialScope::WhileLoop)
		} else {
			InstructionExitState::Goto(self.offset)
		})

	}
}