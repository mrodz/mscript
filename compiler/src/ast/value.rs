use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{
    math_expr::Expr, r#type::IntoType, string::AstString, Compile, CompiledItem, Dependencies,
    Dependency, Function, Ident, Number, TypeLayout,
};

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
    String(AstString),
    MathExpr(Box<Expr>),
}

impl IntoType for Value {
    fn into_type(&self) -> Result<TypeLayout> {
        match self {
            Self::Function(function) => function.clone().consume_for_type(),
            Self::Ident(ident) => Ok(ident.ty()?.clone().into_owned()),
            Self::MathExpr(math_expr) => math_expr.into_type(),
            Self::Number(number) => number.into_type(),
            Self::String(string) => string.into_type(),
        }
    }
}

impl Dependencies for Value {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Function(function) => {
                // println!("\t\t\t\tI depend on a function!!!");
                function.net_dependencies()
            }
            Self::Ident(name) => name.net_dependencies(),
            Self::Number(number) => number.net_dependencies(),
            Self::String(string) => string.net_dependencies(),
            Self::MathExpr(math_expr) => math_expr.net_dependencies(),
        }
    }
}

impl Compile for Value {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::Function(function) => function.compile(function_buffer),
            Self::Ident(ident) => ident.compile(function_buffer),
            Self::Number(number) => number.compile(function_buffer),
            Self::String(string) => string.compile(function_buffer),
            Self::MathExpr(math_expr) => math_expr.compile(function_buffer),
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
            Rule::math_expr => Value::MathExpr(Box::new(Self::math_expr(input)?)),
            x => unreachable!("{x:?}"),
        };

        Ok(matched)
    }
}
