use std::{rc::Rc, collections::HashSet};

use anyhow::Result;
use once_cell::sync::Lazy;

use crate::{Ctx, InstructionExitState, Instruction, variables::ObjectBuilder, rc_to_ref, object};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct MakeObject;

/// This variable allows instructions to register objects on the fly.
static mut OBJECT_BUILDER: Lazy<ObjectBuilder> = Lazy::new(ObjectBuilder::new);

impl Instruction for MakeObject {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let object_variables = Rc::new(context.get_frame_variables().clone());
		let function = context.owner();
        let name = Rc::new(function.name().clone());

		let obj = unsafe {
			if !OBJECT_BUILDER.has_class_been_registered(&name) {
				let location = &function.location();
				let object_path = format!("{}#{name}$", location.path());

				let object_functions = rc_to_ref(location).get_object_functions(&object_path)?;

				let mut mapping: HashSet<String> = HashSet::new();

				for func in object_functions {
					mapping.insert(func.get_qualified_name());
				}

				OBJECT_BUILDER.register_class(Rc::clone(&name), mapping);
			}

			OBJECT_BUILDER.name(Rc::clone(&name)).object_variables(object_variables).build()
		};

		context.push(object!(Rc::new(obj)));

		Ok(InstructionExitState::Processed)

	}
}