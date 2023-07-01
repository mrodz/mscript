use std::{borrow::Cow, ops::Deref};

use anyhow::Result;

use crate::{
    parser::{AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    math_expr::Expr, r#type::IntoType, string::AstString, Callable, Compile, CompiledItem,
    Dependencies, Dependency, Function, Ident, Number, TypeLayout,
};

#[derive(Debug)]
pub(crate) enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
    String(AstString),
    MathExpr(Box<Expr>),
    Callable(Callable),
    Boolean(bool),
}

impl Value {
    pub fn is_callable(&self) -> bool {
        match self {
            Value::Callable(..) => true,
            Value::MathExpr(maybe_value)
                if matches!(maybe_value.deref(), Expr::Value(Value::Callable(..))) =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn associate_with_ident(&self, ident: &mut Ident, user_data: &AssocFileData) -> Result<()> {
        match self {
            Self::Function(ref f) => {
                let ty = f.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Self::Ident(..) => {
                ident.link_from_pointed_type_with_lookup(user_data)?;
            }
            Self::Number(ref number) => {
                let ty = number.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Self::String(ref string) => {
                let ty = string.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Self::MathExpr(ref math_expr) => {
                let ty = math_expr.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Self::Callable(ref callback) => {
                let ty = callback.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Self::Boolean(ref boolean) => {
                let ty = boolean.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
        }

        Ok(())
    }
}

impl IntoType for Value {
    fn for_type(&self) -> Result<TypeLayout> {
        match self {
            Self::Function(function) => function.for_type(),
            Self::Ident(ident) => Ok(ident.ty()?.clone().into_owned()),
            Self::MathExpr(math_expr) => math_expr.for_type(),
            Self::Number(number) => number.for_type(),
            Self::String(string) => string.for_type(),
            Self::Callable(callable) => callable.for_type(),
            Self::Boolean(boolean) => boolean.for_type(),
        }
    }
}

impl Dependencies for Value {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Function(function) => function.net_dependencies(),
            Self::Ident(name) => name.net_dependencies(),
            Self::Number(number) => number.net_dependencies(),
            Self::String(string) => string.net_dependencies(),
            Self::MathExpr(math_expr) => math_expr.net_dependencies(),
            Self::Callable(callable) => callable.net_dependencies(),
            Self::Boolean(boolean) => boolean.net_dependencies(),
        }
    }
}

impl Compile for Value {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::Function(function) => function.in_place_compile_for_value(function_buffer),
            Self::Ident(ident) => ident.compile(function_buffer),
            Self::Number(number) => number.compile(function_buffer),
            Self::String(string) => string.compile(function_buffer),
            Self::MathExpr(math_expr) => math_expr.compile(function_buffer),
            Self::Callable(callable) => callable.compile(function_buffer),
            Self::Boolean(boolean) => boolean.compile(function_buffer),
        }
    }
}

impl Parser {
    pub fn value(input: Node) -> Result<Value, Vec<anyhow::Error>> {
        let matched = match input.as_rule() {
            Rule::function => Value::Function(Self::function(input)?),
            Rule::ident => Value::Ident(Self::ident(input).to_err_vec()?),
            Rule::number => Value::Number(Self::number(input).to_err_vec()?),
            Rule::string => Value::String(Self::string(input).to_err_vec()?),
            Rule::math_expr => Value::MathExpr(Box::new(Self::math_expr(input)?)),
            Rule::callable => Value::Callable(Self::callable(input)?),
            Rule::WHITESPACE => unreachable!("{:?}", input.as_span()),
            x => unreachable!("{x:?}"),
        };

        Ok(matched)
    }
}
