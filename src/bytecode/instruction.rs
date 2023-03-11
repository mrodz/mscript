
use std::collections::{HashSet, HashMap};


use super::variable::{Type, Primitive};

pub type InstructionSignature<'a> = fn (&mut Ctx<'a>) -> ();

mod implementations {
    use super::Ctx;

	pub(crate) fn constexpr(ctx: &mut Ctx) {
		// ctx.push()
	}
}

pub fn get(name: &String) -> InstructionSignature {
	match name {
		_ => panic!("unknown bytecode instruction ({name})")
	}
}

pub struct Ctx<'a> {
	stack: Vec<Primitive>,
	names: HashSet<&'a String>
}

impl <'a>Ctx<'a> {
	fn register_variable(&'a mut self, name: &'a String) {
		self.names.insert(name);
	}

	fn push(&mut self, var: Primitive) {
		self.stack.push(var);
	}
}

pub struct Instruction {
	arguments: Vec<String>,
	children: Vec<Instruction>,
}