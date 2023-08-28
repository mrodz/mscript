use anyhow::{bail, Result};

use crate::{
    context::SpecialScope, instruction_constants::IF, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory},
    Bool, Ctx, Instruction, InstructionExitState,
};

use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct If {
    offset: isize,
}

impl Instruction for If {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        if context.stack_size() == 0 {
            bail!("if statements require at least one entry in the local stack")
        }

        let item = context.pop().unwrap();
        context.clear_stack();

        let Bool(b) = item else {
			bail!("if statement can only test booleans")
		};

        Ok(if b {
            InstructionExitState::PushScope(SpecialScope::If)
        } else {
            InstructionExitState::Goto(self.offset)
        })
    }

    fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);

		Self {
			offset: factory.next_isize()
		}
	}

    fn serialize(&self) -> Vec<u8> {
        SerializedInstructionBuilder::new(IF)
            .add_isize(self.offset)
            .build()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Else;

impl Instruction for Else {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        Ok(InstructionExitState::PushScope(SpecialScope::Else))
    }
}
