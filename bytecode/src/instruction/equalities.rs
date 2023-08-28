use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, bool, serialization::SerializedInstructionBuilder, instruction_constants::{EQU, NEQ}};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Equ;

impl Instruction for Equ {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 2 {
			bail!("equ requires only 2 items in the local stack")
		}

		let first = context.pop().unwrap();
		let second = context.pop().unwrap();

		let result = first.equals(&second)?;

		context.push(bool!(result));

		Ok(InstructionExitState::Processed)
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(EQU).build()
	}

	fn deserialize(bytes: &[u8]) -> Self {
		Self
	}
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Neq;

impl Instruction for Neq {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 2 {
			bail!("neq requires only 2 items in the local stack")
		}

		let first = context.pop().unwrap();
		let second = context.pop().unwrap();

		let result = !first.equals(&second)?;

		context.push(bool!(result));

		Ok(InstructionExitState::Processed)
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(NEQ).build()
	}

	fn deserialize(bytes: &[u8]) -> Self {
		Self
	}
}