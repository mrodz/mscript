use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, Function, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory}, instruction_constants::CALL};

use super::{JumpRequestDestination, JumpRequest};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum Call {
    FromFunctionObject,
    Path(Box<str>),
}

impl Instruction for Call {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let arguments = context.get_local_operating_stack();

		let exit = match self {
			Call::FromFunctionObject => {
				let last = context.pop();

                let Some(Function(f)) = last else {
                    bail!("the last item in the local stack ({last:?}) is not a function.")
                };

                InstructionExitState::JumpRequest(JumpRequest {
                    destination: JumpRequestDestination::Standard(f.location().clone()),
                    callback_state: f.callback_state().clone(),
                    stack: context.rced_call_stack(),
                    arguments,
                })
			}
			Call::Path(path) => {
				InstructionExitState::JumpRequest(JumpRequest {
					destination: JumpRequestDestination::Standard(path.to_string()),
					callback_state: None,
					stack: context.rced_call_stack(),
					arguments,
				})
			}
		};

		context.clear_stack();
		Ok(exit)
	}

	fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);

		let is_path_variant = factory.next_bool();

		if is_path_variant {
			Self::Path(factory.next_str()?.into())
		} else {
			Self::FromFunctionObject
		}
	}
	
	fn serialize(&self) -> Vec<u8> {
		let variant = matches!(self, Self::FromFunctionObject);

		let mut builder = SerializedInstructionBuilder::new(CALL);

		let builder = match self {
			Self::FromFunctionObject => builder.add_bool(false),
			Self::Path(path) => {
				builder
					.add_bool(true)
					.add_str(path)
			}
		};

		builder.build()			
	}
}
