use anyhow::Result;

use crate::{parser::{Parser, Node}, ast::CompiledFunctionId};

use super::{FunctionParameters, Dependencies, FunctionBody, Compile, CompiledItem};

pub static mut FUNCTION_ID: isize = 0;

pub fn name_from_function_id(id: isize) -> String {
	format!("__fn{id}")
}

#[derive(Debug)]
pub struct Function {
    pub arguments: FunctionParameters,
    pub body: FunctionBody,
}

impl Dependencies for Function {
	fn get_dependencies(&self) -> Option<Box<[&super::Ident]>> {
		self.body.get_dependencies()
	}
}



impl Compile for Function {
	fn compile(&self) -> Vec<super::CompiledItem> {
		let mut args = self.arguments.compile();
		let mut body = self.body.compile();

		args.append(&mut body);

		unsafe {
			let x = CompiledItem::Function { id: CompiledFunctionId::Generated(FUNCTION_ID), content: args };			
			FUNCTION_ID += 1;

			vec![x]
		}
	}
}

impl Parser {
	pub fn function(input: Node) -> Result<Function> {
		let mut children = input.children();
		let arguments = children.next().unwrap();
		let function_body = children.next().unwrap();

		let arguments = Self::function_parameters(arguments);
		let body = Self::function_body(function_body)?;

		Ok(Function { arguments, body })
	}
}