use anyhow::Result;

use crate::{
    instruction_constants::{JUMP, JUMP_POP},
    serialization::{InstructionDeserializationFactory, SerializedInstructionBuilder},
    Ctx, Instruction, InstructionExitState,
};

#[derive(Debug)]
pub struct Jump {
    offset: isize,
}

impl Instruction for Jump {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        Ok(InstructionExitState::Goto(self.offset))
    }

    fn deserialize(bytes: &[u8]) -> Self {
        let mut factory = InstructionDeserializationFactory::new(bytes);

        Self {
            offset: factory.next_isize(),
        }
    }

    fn serialize(&self) -> Vec<u8> {
        SerializedInstructionBuilder::new(JUMP)
            .add_isize(self.offset)
            .build()
    }
}

#[derive(Debug)]
pub struct JumpPop {
    offset: isize,
    frames_to_pop: Option<usize>,
}

impl Instruction for JumpPop {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let frames_to_pop = self.frames_to_pop.unwrap_or(1);
        Ok(InstructionExitState::GotoPopScope(
            self.offset,
            frames_to_pop,
        ))
    }

    fn deserialize(bytes: &[u8]) -> Self {
        let mut factory = InstructionDeserializationFactory::new(bytes);
		let offset = factory.next_isize().unwrap();
		let specifies_frames_to_pop = factory.next_bool().unwrap();
		let frames_to_pop = if specifies_frames_to_pop {
			Some(factory.next_usize().unwrap())
		} else {
			None
		};

		Self { offset, frames_to_pop }
    }

    fn serialize(&self) -> Vec<u8> {
        let mut builder = SerializedInstructionBuilder::new(JUMP_POP).add_isize(self.offset);

        builder = if let Some(frames_to_pop) = self.frames_to_pop {
            builder.add_bool(true).add_usize(frames_to_pop)
        } else {
            builder.add_bool(false)
        };

        builder.build()
    }
}
