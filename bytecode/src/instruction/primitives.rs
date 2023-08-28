use anyhow::Result;

use crate::{Ctx, InstructionExitState, Instruction, bool, int, float, byte, bigint, string, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory}, instruction_constants::MAKE_BOOL};

#[derive(Debug)]
pub struct MakeBool(bool);

impl Instruction for MakeBool {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(bool!(self.0));
		Ok(InstructionExitState::Processed)
	}

	fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);
		Self(factory.next_bool())
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(MAKE_BOOL)
			.add_bool(self.0)
			.build()
	}
}

#[derive(Debug)]
pub struct MakeInt(i32);

impl Instruction for MakeInt {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(int!(self.0));
		Ok(InstructionExitState::Processed)
	}
}

#[derive(Debug)]
pub struct MakeFloat(f64);

impl Instruction for MakeFloat {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(float!(self.0));
		Ok(InstructionExitState::Processed)
	}
}

#[derive(Debug)]
pub struct MakeByte(u8);

impl Instruction for MakeByte {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(byte!(self.0));
		Ok(InstructionExitState::Processed)
	}
}

#[derive(Debug)]
pub struct MakeBigInt(i128);

impl Instruction for MakeBigInt {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(bigint!(self.0));
		Ok(InstructionExitState::Processed)
	}
}

#[derive(Debug)]
pub struct MakeStr(String);

impl Instruction for MakeStr {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		context.push(string!(self.0));
		Ok(InstructionExitState::Processed)
	}
}