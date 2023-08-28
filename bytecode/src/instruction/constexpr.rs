use anyhow::Result;

use crate::{Ctx, Instruction, InstructionExitState, Primitive, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory}, instruction_constants::CONSTEXPR};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Constexpr {
    input: Box<str>,
}

impl Instruction for Constexpr {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let var = Primitive::from(self.input.as_ref());

		context.push(var);

		Ok(InstructionExitState::Processed)
    }

    fn deserialize(bytes: &[u8]) -> Self {
        let mut factory = InstructionDeserializationFactory::new(bytes);
        Self {
            input: factory.next_str()?.into()
        }
    }

    fn serialize(&self) -> Vec<u8> {
        SerializedInstructionBuilder::new(CONSTEXPR)
            .add_str(self.input.as_ref())
            .build()
    }
}
