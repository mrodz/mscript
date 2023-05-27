use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{Compile, Dependencies, Dependency, Function, Ident, Number, string::AstString, math_expr::Expr, r#type::IntoType, TypeLayout};

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
    String(AstString),
    MathExpr(Box<Expr>)
}

impl IntoType for Value {
    fn into_type(&self) -> super::TypeLayout {
        match self {
            Self::Function(function) => function.clone().consume_for_type(),
            Self::Ident(ident) => ident.ty().unwrap().clone().into_owned(),
            Self::MathExpr(math_expr) => math_expr.into_type(),
            Self::Number(number) => number.into_type(),
            Self::String(string) => string.into_type(),
        }
    }
}

impl Dependencies for Value {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        match self {
            Self::Function(function) => function.get_dependencies(),
            Self::Ident(name) => name.get_dependencies(),
            Self::Number(number) => number.get_dependencies(),
            Self::String(string) => string.get_dependencies(),
            Self::MathExpr(math_expr) => math_expr.get_dependencies(),
        }
    }
}

impl Compile for Value {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        match self {
            Self::Function(function) => function.compile(),
            Self::Ident(ident) => ident.compile(),
            Self::Number(number) => number.compile(),
            Self::String(string) => string.compile(),
            Self::MathExpr(math_expr) => math_expr.compile(),
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
            Rule::math_expr => Value::MathExpr(Box::new(Self::math_expr(input))),
            x => unreachable!("{x:?}"),
        };

        Ok(matched)
    }
}
