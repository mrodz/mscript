use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Neg;

impl Instruction for Neg {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(val) = context.get_last_op_item_mut() else {
			bail!("neg requires one item on the local operating stack")
		};

		val.negate()?;

		Ok(InstructionExitState::Processed)
	}
}