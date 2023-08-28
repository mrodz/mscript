use anyhow::{Result, bail};

use crate::{Ctx, InstructionExitState, Instruction, Bool};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct StoreSkip {
	name: Box<str>,
	/// If true, will skip when true. If false, will skip when false.
	skip_predicate: bool,
	lines_to_jump: usize,
}

impl Instruction for StoreSkip {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		if context.stack_size() != 1 {
			bail!("store_skip can only store a single item");
		}

		let arg = context.get_last_op_item().unwrap();

		let Bool(val) = arg else {
			bail!("store_skip can only operate on bool (found {arg})");
		};

		if self.skip_predicate {
			// skip if true
			if *val {
				return Ok(InstructionExitState::Goto(self.lines_to_jump.try_into()?));
			}

		} else {
			// skip if false
			if !val {
				return Ok(InstructionExitState::Goto(self.lines_to_jump.try_into()?));
			}
		}

		let arg = context.pop().unwrap().move_out_of_heap_primitive();
		context.register_variable_local(self.name.to_string(), arg)?;

		Ok(InstructionExitState::Processed)
	}
}