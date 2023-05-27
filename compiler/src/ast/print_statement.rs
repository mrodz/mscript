use std::borrow::Cow;

use anyhow::Result;

use crate::{
    ast::TypeLayout,
    instruction,
    parser::{Node, Parser},
};

use super::{Compile, Dependencies, Dependency, Value};

#[derive(Debug, Clone)]
pub struct PrintStatement(Value);

impl Dependencies for PrintStatement {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        self.0.get_dependencies()
    }
}

impl Compile for PrintStatement {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let matched = match &self.0 {
            Value::Function(_) => {
                vec![
                    instruction!(string "<function>"),
                    instruction!(printn '*'),
                    instruction!(void),
                ]
            }
            Value::Ident(ident) => {
                let name_str = ident.name();

                let load_instruction = match ident.ty()? {
                    Cow::Owned(TypeLayout::CallbackVariable(..))
                    | Cow::Borrowed(TypeLayout::CallbackVariable(..)) => {
                        instruction!(load_callback name_str)
                    }
                    _ => instruction!(load name_str),
                };

                vec![
                    load_instruction,
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
			Value::String(content) => {
				let mut string_init = content.compile()?;
				string_init.append(&mut vec![
					instruction!(printn '*'),
                    instruction!(void),
				]);

				string_init
			}
            Value::MathExpr(math_expr) => {
                let mut math_init = math_expr.compile()?;
                math_init.append(&mut vec![
					instruction!(printn '*'),
                    instruction!(void),
				]);

                math_init
            }
        };

        Ok(matched)
    }
}

impl Parser {
    pub fn print_statement(input: Node) -> Result<PrintStatement> {
        let item = input.children().next().unwrap();

        let mut value = Self::value(item)?;

        if let Value::Ident(ref mut ident) = value {
            ident.lookup(input.user_data());
        }

        Ok(PrintStatement(value))
    }
}
