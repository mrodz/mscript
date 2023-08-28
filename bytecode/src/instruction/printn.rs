use anyhow::{Result, Context};

use crate::{Ctx, InstructionExitState, Instruction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum Printn {
	Index(usize),
	All,
}


impl Instruction for Printn {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		log::warn!("The `printn` instruction should not be used. Favor the standard library instead.");

		match self {
			Self::All => {
				let Some(first) = context.get_nth_op_item(0) else {
                    println!();
                    return Ok(InstructionExitState::Processed)
                };

                print!("{first}");
                let operating_stack = context.get_local_operating_stack();

                for var in operating_stack.iter().skip(1) {
                    print!(", {var}")
                }

                #[cfg(feature = "debug")]
                stdout().flush()?;

                println!();
			}
			Self::Index(index) => {
				println!("{}", context.get_nth_op_item(*index).context("nothing at index")?);
			}
		}

		Ok(InstructionExitState::Processed)
	}
}