use anyhow::Result;

use crate::{parser::{Parser, Node}, instruction};

use super::{Dependencies, Value, Compile};

#[derive(Debug)]
pub struct PrintStatement(Value);

impl Dependencies for PrintStatement {
	fn get_dependencies(&self) -> Option<Box<[&super::Ident]>> {
		self.0.get_dependencies()
	}
}

impl Compile for PrintStatement {
	fn compile(&self) -> Vec<super::CompiledItem> {
		match &self.0 {
			Value::Function(_) => {
				vec![
					instruction!(string "<function>"),
					instruction!(printn '*'),
					instruction!(void),
				]
			}
			Value::Ident(ident) => {
				let val = &ident.0;
				vec![
					instruction!(load val),
					instruction!(printn '*'),
					instruction!(void),
				]
			}
			Value::Number(number) => {
				let val = number.to_string();
				vec![
					instruction!(constexpr val),
					instruction!(printn '*'),
					instruction!(void),
				]	
			}

		}
	}
}

impl Parser {
	pub fn print_statement(input: Node) -> Result<PrintStatement> {
		let item = input.children().next().unwrap();

		let value = Self::value(item)?;

		Ok(PrintStatement(value))
	}
}