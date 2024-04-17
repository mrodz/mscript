use std::borrow::Cow;

use anyhow::{bail, Result};

use crate::{
    parser::{AssocFileData, Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{
    map::Map, string::AstString, r#type::IntoType, CompilationState, Compile, CompiledItem, Dependencies, Dependency, Expr, Function, Ident, List, Number, TypeLayout
};

#[derive(Debug)]
pub(crate) enum Value {
    Function(Function),
    Ident(Ident),
    Number(Number),
    String(AstString),
    MathExpr(Box<Expr>),
    Boolean(bool),
    List(List),
    Map(Map)
}

#[derive(Debug)]
pub(crate) enum ValToUsize {
    Ok(usize),
    NotConstexpr,
    NaN,
}

impl Value {
    pub fn nil() -> Self {
        Self::MathExpr(Box::new(Expr::Nil))
    }

    pub fn is_nil(&self) -> bool {
        if let Self::MathExpr(temp) = self {
            matches!(temp.as_ref(), Expr::Nil)
        } else {
            false
        }
    }

    pub fn get_usize(&self) -> Result<ValToUsize> {
        let maybe_evaluable = self.try_constexpr_eval()?;

        if maybe_evaluable.is_impossible() {
            return Ok(ValToUsize::NotConstexpr);
        }

        let number = maybe_evaluable.as_ref().unwrap();

        let Value::Number(number) = number else {
            return Ok(ValToUsize::NaN);
        };

        Ok(ValToUsize::Ok(number.try_into()?))
    }

    pub fn try_negate(&self) -> Result<Option<Self>> {
        match self {
            Self::Number(number) => Ok(number.negate().map(Value::Number)),
            Self::MathExpr(expr) => {
                let x = expr.try_constexpr_eval()?;

                if x.is_impossible() {
                    return Ok(None);
                }

                x.as_ref().unwrap().try_negate()
            }
            _ => Ok(None),
        }
    }
}

#[derive(Debug)]
pub(crate) enum ConstexprEvaluation {
    Impossible,
    Owned(Value),
}

impl ConstexprEvaluation {
    pub fn is_impossible(&self) -> bool {
        matches!(self, Self::Impossible)
    }

    pub fn as_ref(&self) -> Option<&Value> {
        match self {
            Self::Impossible => None,
            Self::Owned(val) => Some(val),
        }
    }

    pub fn into_owned(self) -> Option<Value> {
        match self {
            Self::Impossible => None,
            Self::Owned(val) => Some(val),
        }
    }
}

pub(crate) trait CompileTimeEvaluate {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation>;
}

impl CompileTimeEvaluate for Value {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        match self {
            Self::Number(number) => number.try_constexpr_eval(),
            Self::Boolean(bool) => Ok(ConstexprEvaluation::Owned(Value::Boolean(*bool))),
            Self::String(string) => Ok(ConstexprEvaluation::Owned(Value::String(string.clone()))),
            Self::MathExpr(expr) => expr.try_constexpr_eval(),
            Self::List(list) => list.try_constexpr_eval(),
            _ => Ok(ConstexprEvaluation::Impossible),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Dot;

#[deprecated]
pub(crate) trait Indexable {
    fn output_from_value(&self, value: &Value) -> Result<Cow<'static, TypeLayout>>;
}

impl Value {
    pub fn associate_with_ident(&self, ident: &mut Ident, user_data: &AssocFileData) -> Result<()> {
        match self {
            Value::Function(ref f) => {
                let ty = f.for_type()?.get_owned_type_recursively();
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::Ident(..) => {
                ident.link_from_pointed_type_with_lookup(user_data)?;
            }
            Value::Number(ref number) => {
                let ty = number.for_type()?.get_owned_type_recursively();
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::String(ref string) => {
                let ty = string.for_type()?.get_owned_type_recursively();
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::MathExpr(ref math_expr) => {
                let ty = math_expr.for_type()?.get_owned_type_recursively();

                if let TypeLayout::Optional(None) = ty {
                    bail!(
                        "Hint: specify this optional's type like `{}: TYPE? = nil`",
                        ident.name()
                    )
                }

                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::Boolean(ref boolean) => {
                let ty = boolean.for_type()?.get_owned_type_recursively();
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::List(ref list) => {
                let ty = list.for_type_force_mixed()?.get_owned_type_recursively();
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::Map(ref map) => {
                let ty = map.for_type()?.get_owned_type_recursively();
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
            Self::Boolean(boolean) => boolean.for_type(),
            Self::List(list) => list.for_type(),
            Self::Map(map) => map.for_type(),
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
            Self::Boolean(boolean) => boolean.net_dependencies(),
            Self::List(list) => list.net_dependencies(),
            Self::Map(map) => map.net_dependencies(),
        }
    }
}

impl Compile for Value {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        match self {
            Self::Function(function) => function.in_place_compile_for_value(state),
            Self::Ident(ident) => ident.compile(state),
            Self::Number(number) => number.compile(state),
            Self::String(string) => string.compile(state),
            Self::MathExpr(math_expr) => math_expr.compile(state),
            Self::Boolean(boolean) => boolean.compile(state),
            Self::List(list) => list.compile(state),
            Self::Map(map) => map.compile(state),
        }
    }
}

impl Parser {
    pub fn value(input: Node) -> Result<Value, Vec<anyhow::Error>> {
        let mut children = input.children();

        assert_eq!(input.as_rule(), Rule::value);

        let value = children.next().unwrap();
        let value_span = value.as_span();
        let mut matched = match value.as_rule() {
            Rule::function => Value::Function(Self::function(value)?),
            Rule::ident => Value::Ident(Self::ident(value).to_err_vec()?),
            Rule::number => Value::Number(Self::number(value).to_err_vec()?),
            Rule::string => Value::String(Self::string(value).to_err_vec()?),
            Rule::math_expr => Value::MathExpr(Box::new(Expr::parse(value)?)),
            Rule::list => Value::List(Self::list(value)?),
            Rule::WHITESPACE => unreachable!("{:?}", value.as_span()),
            x => unreachable!("not sure how to handle `{x:?}`"),
        };

        if let ConstexprEvaluation::Owned(new_match) = matched
            .try_constexpr_eval()
            .details(
                value_span,
                &input.user_data().get_source_file_name(),
                "attempting to evaluate this expression at compile time resulted in an error",
            )
            .to_err_vec()?
        {
            matched = new_match;
        }

        Ok(matched)
    }
}
