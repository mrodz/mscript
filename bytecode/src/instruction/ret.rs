use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, ReturnValue};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Ret;

impl Instruction for Ret {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() > 1 {
			bail!("ret can only return a single item");
		}

		let var = context.pop();

		let ret = if let Some(primitive) = var {
			ReturnValue::Value(primitive)
		} else {
			ReturnValue::NoValue
		};

		Ok(InstructionExitState::ReturnValue(ret))
	}
}