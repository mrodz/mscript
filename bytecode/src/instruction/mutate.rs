use anyhow::{Result, bail, Context};

use crate::{Ctx, Instruction, InstructionExitState, Object, rc_to_ref};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct Mutate {
    var_name: Box<str>,
}

impl Instruction for Mutate {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        if context.stack_size() != 2 {
            bail!("mutating an object requires two items in the local operating stack (obj, data)")
        }

        let new_item = context.pop().context("could not pop first item")?;

        let Some(Object(o)) = context.get_last_op_item_mut() else {
            bail!("Cannot perform an object mutation on a non-object")
        };

        // this bypass of Rc protections is messy and should be refactored.
        let var = rc_to_ref(o)
            .has_variable(&self.var_name)
            .context("variable does not exist on object")?;

        if var.0.ty() != new_item.ty() {
            bail!(
                "mismatched types in assignment ({:?} & {:?})",
                var.0.ty(),
                new_item.ty()
            )
        }

        let x = &mut rc_to_ref(&var).0;
        *x = new_item;

		Ok(InstructionExitState::Processed)
    }
}
