use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{Compile, Dependencies, Dependency, Function, Ident, Number, string::AstString};

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
    String(AstString),
}

impl Dependencies for Value {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        match self {
            Self::Function(function) => function.get_dependencies(),
            Self::Ident(name) => name.get_dependencies(),
            Self::Number(number) => number.get_dependencies(),
            Self::String(string) => string.get_dependencies()
        }
    }
}

impl Compile for Value {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        match self {
            Self::Function(function) => function.compile(),
            Self::Ident { .. } => unimplemented!(),
            Self::Number(number) => number.compile(),
            Self::String(string) => string.compile(),
        }
    }
}

impl Parser {
    pub fn value(input: Node) -> Result<Value> {
        let matched = match input.as_rule() {
            Rule::function => Value::Function(Self::function(input)?),
            Rule::ident => {
                let ident = Self::ident(input);
                Value::Ident(ident.clone())
            }
            Rule::number => Value::Number(Self::number(input)?),
            Rule::string => Value::String(Self::string(input)?),
            x => unreachable!("{x:?}"),
        };

        Ok(matched)
    }
}
