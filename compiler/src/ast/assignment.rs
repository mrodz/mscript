use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{value::Value, Ident, Dependencies};

#[derive(Debug)]
pub struct Assignment {
    ident: Ident,
    value: Value,
}

impl Dependencies for Assignment {
    fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
        match self.value {
            Value::Ident(ref ident) => {
                ident.get_dependencies()
            }
            Value::Function(ref function) => {
                function.get_dependencies()
            }
            Value::Number(ref number) => {
                number.get_dependencies()
            }
        }
    }
}

impl Parser {
    pub fn assignment(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let (Some(ident), Some(rhs)) = (children.next(), children.next()) else {
			unreachable!()
		};

        let ident = Self::ident(ident);

        let value = Self::value(rhs)?;

        Ok(Assignment { ident, value })
    }
}
