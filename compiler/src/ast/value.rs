use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{Dependencies, Function, Ident, Number, Compile};



#[derive(Debug)]
pub enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
}

impl Dependencies for Value {
    fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
        match self {
            Self::Function(function) => function.get_dependencies(),
            Self::Ident(ident) => ident.get_dependencies(),
			Self::Number(number) => number.get_dependencies(),
        }
    }
}

impl Compile for Value {
	fn compile(&self) -> Vec<super::CompiledItem> {
		match self {
            Self::Function(function) => function.compile(),
            Self::Ident(ident) => unimplemented!(),
			Self::Number(number) =>  number.compile(),
        }
	}
}

impl Parser {
    pub fn value(input: Node) -> Result<Value> {
        let matched = match input.as_rule() {
            Rule::function => Value::Function(Self::function(input)?),
            Rule::ident => Value::Ident(Self::ident(input)),
            Rule::number => Value::Number(Self::number(input)?),
            x => unreachable!("{x:?}"),
        };

        Ok(matched)
    }
}
