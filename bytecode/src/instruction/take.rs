use std::rc::Rc;

use anyhow::Result;

use crate::{Ctx, Instruction, InstructionExitState, Primitive, stack::VariableFlags};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Take {
    name: String,
}

impl Instruction for Take {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let deleted: Rc<(Primitive, VariableFlags)> = context.delete_variable_local(&self.name)?;
        let primitive: Primitive = deleted.0.clone();

        context.push(primitive);

		Ok(InstructionExitState::Processed)
    }
}
