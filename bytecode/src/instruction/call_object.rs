use anyhow::{Result, bail};

use crate::{context::Ctx, function::InstructionExitState, Object, serialization::{SerializedInstructionBuilder, InstructionDeserializationFactory}, instruction_constants::CALL_OBJECT};

use super::{Instruction, JumpRequestDestination, JumpRequest};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct CallObject {
	method_name: Box<str>,
}

impl Instruction for CallObject {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let Some(first) = context.get_nth_op_item(0) else {
			bail!("there is no item in the local stack")
		};

		let Object(o) = first else {
			bail!("last item in the local stack {first:?} is not an object.")
		};

		let callback_state = Some(o.object_variables.clone());

		let arguments = context.get_local_operating_stack();
		
		context.clear_stack();

		Ok(InstructionExitState::JumpRequest(JumpRequest {
			destination: JumpRequestDestination::Standard(self.method_name.to_string()),
			callback_state,
			stack: context.rced_call_stack(),
			arguments,
		}))
	}

	fn deserialize(bytes: &[u8]) -> Self {
		let mut factory = InstructionDeserializationFactory::new(bytes);
		Self {
			method_name: factory.next_str(),
		}
	}

	fn serialize(&self) -> Vec<u8> {
		SerializedInstructionBuilder::new(CALL_OBJECT)
			.add_str(self.method_name.as_ref())
			.build()
	}
}