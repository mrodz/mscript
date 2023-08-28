use anyhow::{bail, Result};

use crate::{
    instruction_constants::DELETE, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory}, Ctx, Instruction,
    InstructionExitState,
};

use serde_derive::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Delete {
    names: Vec<Box<str>>,
}

impl Instruction for Delete {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        if self.names.is_empty() {
            bail!("delete requires names to delete")
        };

        for name in self.names.iter() {
            context.delete_variable_local(name)?;
        }

        Ok(InstructionExitState::Processed)
    }

    fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);
		let mut strings = vec![];
		
		while factory.bytes_remaining() != 0 {
			strings.push(factory.next_str()?);
		}

		Self { names: strings }
	}

    fn serialize(&self) -> Vec<u8> {
        let names_len = self.names.len();

        let mut builder = SerializedInstructionBuilder::new(DELETE);

        for name in self.names.iter() {
			builder = builder.add_str(name.as_ref())
		}

		builder.build()
    }
}
