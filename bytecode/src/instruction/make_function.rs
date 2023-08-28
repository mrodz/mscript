use std::{collections::HashMap, rc::Rc};

use anyhow::{Result, bail};

use crate::stack::VariableMapping;
use crate::{Ctx, InstructionExitState, Instruction, function, function::PrimitiveFunction};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct MakeFunction {
	path_to_call: Box<str>,
	variables_captured: Box<[Box<str>]>
}

impl Instruction for MakeFunction {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		let callback_pool: Option<Rc<VariableMapping>> = if !self.variables_captured.is_empty() {
			let mut map = HashMap::with_capacity(self.variables_captured.len());
			for var_name in self.variables_captured.iter() {
				let Some(var) = context.load_variable(var_name) else {
					bail!("{var_name} is not in scope")
				};

				map.insert(var_name.to_string(), var.clone());
			}

			Some(Rc::new(map.into()))
		} else {
			None
		};

		context.push(function!(PrimitiveFunction::new(self.path_to_call.to_string(), callback_pool)));

		Ok(InstructionExitState::Processed)
	}
}