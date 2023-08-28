use anyhow::{bail, Result};

use crate::{Bool, Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Not;

impl Instruction for Not {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(val) = context.get_last_op_item_mut() else {
			bail!("not requires one item on the local operating stack")
		};

		let Bool(val) = val else {
			bail!("not can only negate booleans")
		};

		*val = !*val;

		Ok(InstructionExitState::Processed)
	}
}