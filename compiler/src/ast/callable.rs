use std::borrow::Cow;

use anyhow::Result;

use crate::{parser::{Parser, Node}, instruction};

use super::{Ident, FunctionArguments, Dependencies, Compile, CompiledItem, Dependency, TypeLayout};

#[derive(Debug, Clone)]
pub struct Callable {
	ident: Ident,
	function_arguments: FunctionArguments,
}

impl Compile for Callable {
	fn compile(&self) -> Result<Vec<CompiledItem>> {
		let mut args: Vec<CompiledItem> = self.function_arguments.iter()
			.flat_map(|x| x.compile().unwrap())
			// .flatten()
			.collect();

		let func_name = self.ident.name();
		let ident = &self.ident;
		
		let load_instruction = match ident.ty()? {
			Cow::Owned(TypeLayout::CallbackVariable(..)) | Cow::Borrowed(TypeLayout::CallbackVariable(..)) => instruction!(load_callback func_name),
			_ => instruction!(load func_name)
		};

		args.push(load_instruction);
		args.push(instruction!(call));

		Ok(args)
	}
}

impl Dependencies for Callable {
	fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
		// a call needs to have access to the function/object
		let maybe_arg_dependencies = self.function_arguments.get_dependencies();

		if let Some(arg_dependencies) = maybe_arg_dependencies {
			let mut arg_dependencies = arg_dependencies.into_vec();
			
			arg_dependencies.push(Dependency::new(Cow::Borrowed(&self.ident)));

			Some(arg_dependencies.into_boxed_slice())
		} else {
			Some([Dependency::new(Cow::Borrowed(&self.ident))].into())	
		}
	}
}

impl Parser {
	pub fn callable(input: Node) -> Result<Callable> {
		let mut children = input.children();

		let ident = children.next().unwrap();

		let user_data = input.user_data();

		let mut ident = Self::ident(ident);
		ident.link_from_pointed_type_with_lookup(user_data)?;
		// ident.link(user_data, None);

		// let (ident, inherited) = Self::ident(ident).link(user_data, None);

		// if !inherited {
		// 	panic!("cannot get type of callable member {ident:?}")
		// }

		let function_arguments = children.next().unwrap();
		let function_arguments = Self::function_arguments(function_arguments)?;

		Ok(Callable { ident, function_arguments })
	}
}