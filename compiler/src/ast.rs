mod assignment;
mod callable;
mod declaration;
mod function;
mod function_arguments;
mod function_body;
mod function_parameters;
mod ident;
mod number;
mod print_statement;
mod value;

use std::fmt::Display;

pub use assignment::Assignment;
pub use callable::Callable;
pub use declaration::Declaration;
pub use function::Function;
pub use function_arguments::FunctionArguments;
pub use function_body::FunctionBody;
pub use function_parameters::FunctionParameters;
pub use ident::Ident;
pub use number::Number;
pub use print_statement::PrintStatement;
pub use value::Value;

use self::function::name_from_function_id;

#[derive(Debug)]
pub enum CompiledFunctionId {
	Generated(isize),
	Custom(String)
}

impl Display for CompiledFunctionId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CompiledFunctionId::Custom(string) => write!(f, "{string}"),
			CompiledFunctionId::Generated(id) => write!(f, "{}", name_from_function_id(*id))
		}
	}
}

#[derive(Debug)]
pub enum CompiledItem {
    Function {
		id: CompiledFunctionId,
		content: Vec<CompiledItem>,
	},
    Instruction {
		id: u8,
		arguments: Box<[String]>,
	},
}

impl CompiledItem {
	pub fn repr(&self) -> String {
		match self {
			Self::Function { id, content } => {
				let content: String = content.iter().map(|x| x.repr()).collect();

				todo!()
			}
			Self::Instruction { id, arguments } => {
				let mut args = String::new();
				
				if arguments.len() >= 1 {
					for arg in &arguments[..] {
						args.push(' ');
						if arg.contains(' ') {
							args.push('\"');
							args.push_str(&arg);
							args.push('\"');
						} else {
							args.push_str(&arg);
						}
					}
				}
		
				format!("{}{}\0", *id as char, args)
			}
		}
	}
}

#[macro_export]
macro_rules! instruction {
	($name:tt $($arg:tt)*) => {{
		use crate::ast::*;

		let id = bytecode::compilation_lookups::string_instruction_representation_to_byte(stringify!($name))
			.expect("instruction does not exist");

		let mut arguments = vec![];

		$(
			arguments.push($arg.to_string());
		)*

		let arguments = arguments.into_boxed_slice();

		CompiledItem::Instruction { id: *id, arguments }

	}};
}



pub(crate) trait Compile {
    fn compile(&self) -> Vec<CompiledItem>;
}

pub(crate) trait Optimize {}

pub(crate) trait Dependencies {
    fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
        None
    }
}
