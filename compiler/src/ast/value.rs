use std::borrow::Cow;

use anyhow::Result;

use crate::{
    parser::{AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    math_expr::Expr, new_err, r#type::IntoType, string::AstString, CompilationState, Compile,
    CompiledItem, Dependencies, Dependency, Function, Ident, List, Number, TypeLayout,
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
}

pub(crate) enum ValToUsize {
    Ok(usize),
    NotConstexpr,
    NaN,
}

impl Value {
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
            Self::Number(number) => Ok(ConstexprEvaluation::Owned(Value::Number(number.clone()))),
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

#[cfg(not)]
#[derive(Debug)]
pub(crate) enum ValueChainType {
    Index(Index),
    #[allow(unused)]
    Dot(Dot), // todo
}

#[cfg(not)]
impl CompileTimeEvaluate for Value {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        if let Some(ref next) = self.1 {
            let val_constexpr: ConstexprEvaluation = self.0.try_constexpr_eval()?;
            let Some(val_constexpr) = val_constexpr.as_ref() else {
                return Ok(ConstexprEvaluation::Impossible);
            };

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

                            let indexed =
                                list.get_value(numeric_idx).context("index out of bounds")?;

                            indexed.try_constexpr_eval()
                        }
                        Number::Float(_) => bail!("cannot index into a list with floats"),
                    }
                }
                ValueChainType::Dot(_) => unreachable!(),
            }
        } else {
            self.0.try_constexpr_eval()
        }
    }
}

#[cfg(not)]
impl Dependencies for ValueChainType {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Index(index) => index.net_dependencies(),
            Self::Dot(_) => unimplemented!(),
        }
    }
}

#[cfg(not)]
impl Indexable for ValueChainType {
    fn output_from_value(&self, value: &Value) -> Result<Cow<'static, TypeLayout>> {
        match self {
            Self::Index(index) => index.output_from_value(value),
            Self::Dot(_) => unimplemented!(),
        }
    }
}

#[cfg(not)]
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

// #[derive(Debug)]
// pub(crate) struct ValueChain(pub(crate) Value, pub(crate) Option<Box<ValueChainType>>);

// impl Dependencies for ValueChain {
//     fn dependencies(&self) -> Vec<Dependency> {
//         let mut value_dependencies = self.0.net_dependencies();
//         if let Some(ref next) = self.1 {
//             value_dependencies.append(&mut next.net_dependencies());
//         }
//         value_dependencies
//     }
// }

// impl IntoType for ValueChain {
//     fn for_type(&self) -> Result<TypeLayout> {
//         if let Some(ref other) = self.1 {
//             other.output_from_value(&self.0).map(|x| x.into_owned())
//         } else {
//             self.0.for_type()
//         }
//     }
// }

impl Value {
    // #[allow(unused)]
    // pub fn is_value_callable(&self) -> bool {
    //     match &self.0 {
    //         Value::Callable(..) => true,
    //         Value::MathExpr(maybe_value)
    //             if matches!(maybe_value.deref(), Expr::Value(Value::Callable(..))) =>
    //         {
    //             true
    //         }
    //         _ => false,
    //     }
    // }

    // pub fn add_index(&mut self, index: Index) {
    //     if let Some(_) = self.1 {
    //         unreachable!("index already there!")
    //     } else {
    //         self.1 = Some(Box::new(ValueChainType::Index(index)))
    //     }
    // }

    pub fn is_callable(&self) -> Result<bool> {
        let ty = self.for_type()?;

        Ok(ty.is_function().is_some())
    }

    pub fn associate_with_ident(&self, ident: &mut Ident, user_data: &AssocFileData) -> Result<()> {
        match self {
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
            Value::Boolean(ref boolean) => {
                let ty = boolean.for_type()?;
                ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
            }
            Value::List(ref list) => {
                let ty = list.for_type_force_mixed()?;
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
            // Self::Callable(callable) => callable.net_dependencies(),
            Self::Boolean(boolean) => boolean.net_dependencies(),
            Self::List(list) => list.net_dependencies(),
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
        }
    }
}

#[cfg(not)]
impl Compile for Value {
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
    pub fn value(input: Node) -> Result<Value, Vec<anyhow::Error>> {
        let mut children = input.children();

        let value = children.next().unwrap();
        let matched = match value.as_rule() {
            Rule::function => Value::Function(Self::function(value)?),
            Rule::ident => Value::Ident(Self::ident(value).to_err_vec()?),
            Rule::number => Value::Number(Self::number(value).to_err_vec()?),
            Rule::string => Value::String(Self::string(value).to_err_vec()?),
            Rule::math_expr => Value::MathExpr(Box::new(Expr::parse(value)?)),
            // Rule::callable => Value::Callable(Self::callable(value)?),
            Rule::list => Value::List(Self::list(value)?),
            Rule::WHITESPACE => unreachable!("{:?}", value.as_span()),
            x => {
                return Err(vec![new_err(
                    input.as_span(),
                    &input.user_data().get_source_file_name(),
                    format!("not sure how to handle `{x:?}` :("),
                )])?
            }
        };

        Ok(matched)
    }
}
