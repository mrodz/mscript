use anyhow::Result;

use crate::{Ctx, Instruction, InstructionExitState, instruction_constants::CALL_LIB_BYTE, SerializedInstructionBuilder, serialization::InstructionDeserializationFactory};

use super::{JumpRequestDestination, JumpRequest};

#[derive(Debug)]
pub struct CallLib {
    library_path: Box<str>,
    symbol_name: Box<str>,
}

impl Instruction for CallLib {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let arguments = context.get_local_operating_stack();

        let exit = InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Library {
                lib_name: self.library_path.to_string(),
                func_name: self.symbol_name.to_string(),
            },
            // destination_label: JumpRequestDestination::Standard(first.clone()),
            callback_state: None,
            stack: context.rced_call_stack(),
            arguments,
        });

        context.clear_stack();

		Ok(exit)
    }

    fn deserialize(bytes: &[u8]) -> Self {
        let mut factory = InstructionDeserializationFactory::new(bytes);

        Self {
            library_path: factory.next::<&str>().into(),
            symbol_name: factory.next::<&str>().into(),
        }
    }

    fn serialize(&self) -> Vec<u8> {
        SerializedInstructionBuilder::new(CALL_LIB_BYTE)
            .add(&self.library_path)
            .add(&self.symbol_name)
            .build()
    }
}
