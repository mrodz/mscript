use std::{borrow::Cow, ops::Deref};

use anyhow::{Result, bail, Context};

use crate::{
    parser::{AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    list::Index, math_expr::Expr, r#type::IntoType, string::AstString, Callable, Compile,
    CompiledItem, Dependencies, Dependency, Function, Ident, List, Number, TypeLayout,
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
    List(List),
}


impl Value {
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
            _ => Ok(None)
        }
    }
}

pub(crate) enum ConstexprEvaluation {
    Impossible,
    Owned(Value)
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
}

pub(crate) trait CompileTimeEvaluate {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation>;
}

impl <'a>CompileTimeEvaluate for Value {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        match self {
            Self::Number(number) => Ok(ConstexprEvaluation::Owned(Value::Number(number.clone()))),
            Self::Boolean(bool) => Ok(ConstexprEvaluation::Owned(Value::Boolean(bool.clone()))),
            Self::String(string) => Ok(ConstexprEvaluation::Owned(Value::String(string.clone()))),
            Self::MathExpr(expr) => {
                expr.try_constexpr_eval()
            }
            _ => Ok(ConstexprEvaluation::Impossible),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Dot;

pub(crate) trait Indexable {
    fn output_from_value(&self, value: &Value) -> Result<Cow<'static, TypeLayout>>;
}

#[derive(Debug)]
pub(crate) enum ValueChainType {
    Index(Index),
    #[allow(unused)]
    Dot(Dot), // todo
}

impl CompileTimeEvaluate for ValueChain {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        if let Some(ref next) = self.1 {
            let val_constexpr: ConstexprEvaluation = self.0.try_constexpr_eval()?;
            let Some(val_constexpr) = val_constexpr.as_ref() else {
                return Ok(ConstexprEvaluation::Impossible);
            };

            // let val_constexpr: &'a Value = val_constexpr;

            match next.as_ref() {
                ValueChainType::Index(index) => {
                    let Value::List(list) = val_constexpr else {
                        unreachable!();
                    };

                    let index_constexpr = index.try_constexpr_eval()?;
                    let Some(index_constexpr) = index_constexpr.as_ref() else {
                        return Ok(ConstexprEvaluation::Impossible);
                    };

                    let Value::Number(number) = index_constexpr else {
                        bail!("can only index into lists with numbers")
                    };

                    match number {
                        Number::BigInt(idx) | Number::Integer(idx) | Number::Byte(idx) => {
                            let numeric_idx: usize = idx.parse()?;
    
                            dbg!(numeric_idx);
    
                            let indexed = list.get_value(numeric_idx).context("index out of bounds")?;

                            return indexed.try_constexpr_eval()
                        }
                        Number::Float(_) => bail!("cannot index into a list with floats"),
                    }
                }
                ValueChainType::Dot(_) => unreachable!()
            }
        } else {
            self.0.try_constexpr_eval()
        }
    }
}

impl Dependencies for ValueChainType {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Index(index) => index.net_dependencies(),
            Self::Dot(_) => unimplemented!(),
        }
    }
}

impl Indexable for ValueChainType {
    fn output_from_value(&self, value: &Value) -> Result<Cow<'static, TypeLayout>> {
        match self {
            Self::Index(index) => index.output_from_value(value),
            Self::Dot(_) => unimplemented!(),
        }
    }
}

impl Compile for ValueChainType {
    fn compile(
        &self,
        function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>, anyhow::Error> {
        match self {
            Self::Index(index) => index.compile(function_buffer),
            Self::Dot(_) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct ValueChain(pub(crate) Value, pub(crate) Option<Box<ValueChainType>>);

impl Dependencies for ValueChain {
    fn dependencies(&self) -> Vec<Dependency> {
        let mut value_dependencies = self.0.net_dependencies();
        if let Some(ref next) = self.1 {
            value_dependencies.append(&mut next.net_dependencies());
        }
        value_dependencies
    }
}

impl IntoType for ValueChain {
    fn for_type(&self) -> Result<TypeLayout> {
        if let Some(ref other) = self.1 {
            other.output_from_value(&self.0).map(|x| x.into_owned())
        } else {
            self.0.for_type()
        }
    }
}

impl ValueChain {
    #[allow(unused)]
    pub fn is_value_callable(&self) -> bool {
        match &self.0 {
            Value::Callable(..) => true,
            Value::MathExpr(maybe_value)
                if matches!(maybe_value.deref(), Expr::Value(Value::Callable(..))) =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn is_callable(&self) -> Result<bool> {
        let ty = self.for_type()?;

        return Ok(ty.is_function().is_some());
    }

    pub fn associate_with_ident(&self, ident: &mut Ident, user_data: &AssocFileData) -> Result<()> {
        if let Some(next) = self.1.as_deref() {
            let ty = next.output_from_value(&self.0)?;
            ident.link_force_no_inherit(user_data, ty)?;
        } else {
            match self.0 {
                Value::Function(ref f) => {
                    let ty = f.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::Ident(..) => {
                    ident.link_from_pointed_type_with_lookup(user_data)?;
                }
                Value::Number(ref number) => {
                    let ty = number.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::String(ref string) => {
                    let ty = string.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::MathExpr(ref math_expr) => {
                    let ty = math_expr.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::Callable(ref callback) => {
                    let ty = callback.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::Boolean(ref boolean) => {
                    let ty = boolean.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::List(ref list) => {
                    let ty = list.for_type_force_mixed()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
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
            Self::List(list) => list.for_type(),
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
            Self::List(list) => list.net_dependencies(),
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
            Self::List(list) => list.compile(function_buffer),
        }
    }
}

impl Compile for ValueChain {
    fn compile(
        &self,
        function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let mut value_compiled = self.0.compile(function_buffer)?;

        if let Some(ref next) = self.1 {
            value_compiled.append(&mut next.compile(function_buffer)?);
        }

        Ok(value_compiled)
    }
}

impl Parser {
    pub fn value(input: Node) -> Result<ValueChain, Vec<anyhow::Error>> {
        let mut children = input.children();

        let value = children.next().unwrap();
        let matched = match value.as_rule() {
            Rule::function => Value::Function(Self::function(value)?),
            Rule::ident => Value::Ident(Self::ident(value).to_err_vec()?),
            Rule::number => Value::Number(Self::number(value).to_err_vec()?),
            Rule::string => Value::String(Self::string(value).to_err_vec()?),
            Rule::math_expr => Value::MathExpr(Box::new(Self::math_expr(value)?)),
            Rule::callable => Value::Callable(Self::callable(value)?),
            Rule::list => Value::List(Self::list(value)?),
            Rule::WHITESPACE => unreachable!("{:?}", value.as_span()),
            x => unreachable!("{x:?}"),
        };

        let next = children.next();

        let chain = if let Some(next) = next {
            let x = match next.as_rule() {
                Rule::list_index => ValueChainType::Index(Self::list_index(next)?),
                other => unreachable!("{other:?}"),
            };

            Some(Box::new(x))
        } else {
            None
        };

        Ok(ValueChain(matched, chain))
    }
}
