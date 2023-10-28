//! The official Pest Book implements many of the the exact features needed.
//! Some of this code is copied from their Github. See [this folder](https://github.com/pest-parser/book/blob/42a2889c6057b1192d2b1682b6cc53ff13799a34/examples/pest-calculator/src/main.rs)
//! for the exact code.
//!
//! Huge thanks to the authors for supplying such a useful resource!
//!
//! BEGIN LICENSE
//!
//! MIT:
//! Permission is hereby granted, free of charge, to any
//! person obtaining a copy of this software and associated
//! documentation files (the "Software"), to deal in the
//! Software without restriction, including without
//! limitation the rights to use, copy, modify, merge,
//! publish, distribute, sublicense, and/or sell copies of
//! the Software, and to permit persons to whom the Software
//! is furnished to do so, subject to the following
//! conditions:
//!
//! The above copyright notice and this permission notice
//! shall be included in all copies or substantial portions
//! of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
//! ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
//! TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
//! PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
//! SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
//! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
//! OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
//! IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
//! DEALINGS IN THE SOFTWARE.
//!
//! END LICENSE

use std::{borrow::Cow, rc::Rc, fmt::Display};

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Span,
};

use crate::{
    ast::{number, Callable, ConstexprEvaluation},
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{
    boolean::boolean_from_str, dot_lookup::DotChain, function::FunctionType, list::Index, map_err,
    new_err, r#type::IntoType, ClassType, CompilationState, Compile, CompileTimeEvaluate,
    CompiledItem, Dependencies, Dependency, FunctionArguments, TemporaryRegister, TypeLayout,
    Value,
};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    use pest::pratt_parser::{Assoc::*, Op};
    use Rule::*;

    macro_rules! infix {
        ($rule:ident) => {
            Op::infix($rule, Left)
        };
    }

    PrattParser::new()
        .op(infix!(unwrap))
        .op(infix!(or) | infix!(xor))
        .op(infix!(and))
        .op(infix!(lt)
            | infix!(lte)
            | infix!(gt)
            | infix!(gte)
            | infix!(eq)
            | infix!(neq))
        .op(Op::prefix(not))
        .op(infix!(add) | infix!(subtract))
        .op(infix!(multiply) | infix!(divide) | infix!(modulo))
        .op(Op::prefix(unary_minus))
        .op(Op::postfix(dot_chain)
            | Op::postfix(list_index)
            | Op::postfix(callable)
            | Op::postfix(dot_chain))
        .op(infix!(add_assign) | infix!(sub_assign) | infix!(mul_assign) | infix!(div_assign) | infix!(mod_assign))
});

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,
    And,
    Or,
    Xor,
    Unwrap,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

impl Op {
    pub const fn symbol(&self) -> &'static str {
        match self {
            Op::Add => "+",
            Op::Subtract => "-",
            Op::Multiply => "*",
            Op::Divide => "/",
            Op::Modulo => "%",
            Op::Lt => "<",
            Op::Lte => "<=",
            Op::Gt => ">",
            Op::Gte => ">=",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::And => "&&",
            Op::Or => "||",
            Op::Xor => "^",
            Op::Unwrap => "?=",
            Op::AddAssign => "+=",
            Op::SubAssign => "-=",
            Op::MulAssign => "*=",
            Op::DivAssign => "/=",
            Op::ModAssign => "%=",
        }
    }

    pub const fn is_op_assign(&self) -> bool {
        use Op::*;
        matches!(self, AddAssign | SubAssign | MulAssign | DivAssign | ModAssign)
    }
}

#[derive(Debug)]
pub(crate) enum CallableContents {
    ToSelf {
        return_type: Option<Cow<'static, TypeLayout>>,
        arguments: FunctionArguments,
    },
    Standard {
        lhs_raw: Box<Expr>,
        function: FunctionType,
        arguments: FunctionArguments,
    },
}

#[derive(Debug)]
pub(crate) enum Expr {
    Nil,
    Value(Value),
    ReferenceToSelf,
    ReferenceToConstructor(ClassType),
    UnaryMinus(Box<Expr>),
    UnaryNot(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
    Callable(CallableContents),
    DotLookup {
        lhs: Box<Expr>,
        dot_chain: DotChain,
        expected_type: TypeLayout,
    },
    Index {
        lhs_raw: Box<Expr>,
        index: Index,
    },
}

impl Expr {
    pub(crate) fn validate(&self) -> Result<&Self> {
        self.for_type().map(|_| self)
    }

    pub(crate) fn parse(input: Node) -> Result<Expr, Vec<anyhow::Error>> {
        let children_as_pairs = input.children().into_pairs();
        parse_expr(children_as_pairs, input.user_data().clone())
    }
}

impl CompileTimeEvaluate for Expr {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        self.validate()?;

        match self {
            Self::Value(val) => val.try_constexpr_eval(),
            Self::UnaryNot(expr) => {
                let maybe_constexpr_eval = expr.try_constexpr_eval()?;
                if maybe_constexpr_eval.is_impossible() {
                    return Ok(ConstexprEvaluation::Impossible);
                }

                let constexpr_eval = maybe_constexpr_eval.as_ref().unwrap();

                if let Value::Boolean(b) = constexpr_eval {
                    Ok(ConstexprEvaluation::Owned(Value::Boolean(!*b)))
                } else {
                    Ok(ConstexprEvaluation::Impossible)
                }
            }
            Self::UnaryMinus(expr) => {
                let maybe_constexpr_eval = expr.try_constexpr_eval()?;
                if maybe_constexpr_eval.is_impossible() {
                    return Ok(ConstexprEvaluation::Impossible);
                }

                let constexpr_eval = maybe_constexpr_eval.as_ref().unwrap();

                if !constexpr_eval.for_type()?.supports_negate() {
                    let val = constexpr_eval.try_negate()?;
                    if let Some(val) = val {
                        return Ok(ConstexprEvaluation::Owned(val));
                    }
                }

                Ok(ConstexprEvaluation::Impossible)
            }
            Self::BinOp { lhs, op, rhs } => {
                let lhs = lhs.try_constexpr_eval()?;
                let rhs = rhs.try_constexpr_eval()?;

                let (Some(Value::Number(lhs)), Some(Value::Number(rhs))) =
                    (lhs.as_ref(), rhs.as_ref())
                else {
                    return Ok(ConstexprEvaluation::Impossible);
                };

                let bin_op_applied = match op {
                    Op::Add => lhs + rhs,
                    Op::Subtract => lhs + rhs,
                    Op::Multiply => lhs * rhs,
                    Op::Divide => lhs / rhs,
                    Op::Modulo => lhs % rhs,
                    _ => return Ok(ConstexprEvaluation::Impossible),
                };

                Ok(ConstexprEvaluation::Owned(Value::Number(bin_op_applied?)))
            }
            Self::ReferenceToSelf => Ok(ConstexprEvaluation::Impossible),
            Self::ReferenceToConstructor(..) => Ok(ConstexprEvaluation::Impossible),
            Self::Nil => Ok(ConstexprEvaluation::Impossible),
            Self::Callable { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::Index { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::DotLookup { .. } => Ok(ConstexprEvaluation::Impossible),
        }
    }
}

impl IntoType for Expr {
    fn for_type(&self) -> Result<super::TypeLayout> {
        match self {
            Expr::Value(val) => val.for_type(),
            Expr::BinOp { lhs, op, rhs } => {
                if op.is_op_assign() {
                    let Expr::Value(Value::Ident(ident)) = lhs.as_ref() else {
                        unreachable!()
                    };

                    if ident.is_const() {
                        bail!("cannot reassign using {op} to {}, which is const", ident.name())
                    }
                }

                let lhs = lhs.for_type()?;
                let rhs = rhs.for_type()?;

                lhs.get_output_type(&rhs, op).with_context(|| {
                    format!(
                        "invalid operation: {} {} {rhs}",
                        lhs.get_type_recursively(),
                        op.symbol()
                    )
                })
            }
            Expr::UnaryMinus(val) | Expr::UnaryNot(val) => val.for_type(),
            Expr::Callable(CallableContents::Standard { function, .. }) => {
                let return_type = function
                    .return_type()
                    .get_type()
                    .context("function returns void")?;

                Ok(return_type.clone().into_owned())
            }
            Expr::Callable(CallableContents::ToSelf { return_type, .. }) => {
                let Some(return_type) = return_type else {
                    bail!("function returns void")
                };

                Ok(return_type.clone().into_owned())
            }
            Expr::ReferenceToSelf => Ok(TypeLayout::ClassSelf),
            Expr::ReferenceToConstructor(class_type) => {
                Ok(TypeLayout::Function(class_type.constructor()))
            }
            Expr::Index { index, .. } => index.for_type(),
            Expr::DotLookup { expected_type, .. } => Ok(expected_type.to_owned()),
            Expr::Nil => Ok(TypeLayout::Optional(None)),
        }
    }
}

impl Dependencies for Expr {
    fn dependencies(&self) -> Vec<Dependency> {
        use Expr as E;
        match self {
            E::Value(val) => val.net_dependencies(),
            E::UnaryMinus(expr) | E::UnaryNot(expr) => expr.net_dependencies(),
            E::BinOp { lhs, rhs, .. } => {
                let mut lhs_dep = lhs.net_dependencies();
                let mut rhs_dep = rhs.net_dependencies();

                lhs_dep.append(&mut rhs_dep);

                lhs_dep
            }
            E::Callable(CallableContents::ToSelf { arguments, .. }) => arguments.net_dependencies(),
            E::Callable(CallableContents::Standard {
                lhs_raw, arguments, ..
            }) => {
                let mut lhs_deps = lhs_raw.net_dependencies();
                lhs_deps.append(&mut arguments.net_dependencies());
                lhs_deps
            }
            E::Index { lhs_raw, index } => {
                let mut lhs_deps = lhs_raw.net_dependencies();
                lhs_deps.append(&mut index.net_dependencies());
                lhs_deps
            }
            E::DotLookup { lhs, .. } => lhs.net_dependencies(),
            E::ReferenceToSelf => vec![],
            E::ReferenceToConstructor(..) => vec![],
            E::Nil => vec![],
        }
    }
}

fn compile_depth(
    expr: &Expr,
    state: &CompilationState,
    depth: TemporaryRegister,
) -> Result<Vec<CompiledItem>> {
    match expr {
        Expr::Value(val) => val.compile(state),
        Expr::UnaryNot(expr) => {
            if let Expr::Value(value) = expr.as_ref() {
                let ty = value.for_type()?;

                if !ty.is_boolean() {
                    bail!("cannot apply binary not to {:?}", ty)
                }
            }

            let mut eval = expr.compile(state)?;

            eval.push(instruction!(not));

            Ok(eval)
        }
        Expr::UnaryMinus(expr) => {
            if let Expr::Value(value) = expr.as_ref() {
                match value {
                    Value::Ident(ident) => {
                        let x = ident.ty().context("no type data")?;
                        if !x.supports_negate() {
                            bail!("cannot negate")
                        }
                    }
                    Value::Number(..) => (),
                    _ => bail!("cannot negate"),
                }
            }

            let mut eval = expr.compile(state)?;

            eval.push(instruction!(neg));

            Ok(eval)
        }
        Expr::BinOp { lhs: lhs_raw, op, rhs } => {
            let mut lhs = compile_depth(lhs_raw, state, state.poll_temporary_register())?;
            let mut rhs = compile_depth(rhs, state, state.poll_temporary_register())?;

            match op {
                Op::And => {
                    let rhs_len = rhs.len() + 3;
                    lhs.push(instruction!(store_skip depth "0" rhs_len));
                    lhs.append(&mut rhs);
                    lhs.push(instruction!(load_fast depth));
                    lhs.push(instruction!(bin_op "&&"));
    
                    return Ok(lhs);
                }
                Op::Or => {
                    let rhs_len = rhs.len() + 3;
                    lhs.push(instruction!(store_skip depth "1" rhs_len));
                    lhs.append(&mut rhs);
                    lhs.push(instruction!(load_fast depth));
                    lhs.push(instruction!(bin_op "||"));
    
                    return Ok(lhs);
                }
                Op::AddAssign | Op::SubAssign | Op::MulAssign | Op::DivAssign | Op::ModAssign => {
                    let Expr::Value(Value::Ident(ident)) = lhs_raw.as_ref() else {
                        unreachable!()
                    };

                    // lhs.push(instruction!(store_fast depth));
                    rhs.push(instruction!(bin_op_assign (ident.name()) (op.symbol())));

                    return Ok(rhs)
                }
                _ => {
                    // store the initialization to the "second part", prep for bin_op

                    lhs.push(instruction!(store_fast depth));
                    lhs.append(&mut rhs);
                    lhs.push(instruction!(load_fast depth));
                    lhs.push(instruction!(fast_rev2));        
                }
            }
            
            match op {
                Op::Eq => lhs.push(instruction!(equ)),
                Op::Neq => lhs.push(instruction!(neq)),
                _ => {
                    let symbol = op.symbol();
                    lhs.push(instruction!(bin_op symbol))
                }
            }

            Ok(lhs)
        }
        Expr::Callable(CallableContents::Standard {
            lhs_raw, arguments, ..
        }) => {
            let mut lhs_compiled = lhs_raw.compile(state)?;
            let lhs_register = state.poll_temporary_register();

            lhs_compiled.push(instruction!(store_fast lhs_register));

            let callable: Callable<'_> =
                Callable::new(arguments, instruction!(load_fast lhs_register), None);

            lhs_compiled.append(&mut callable.compile(state)?);

            Ok(lhs_compiled)
        }
        Expr::ReferenceToSelf => Ok(vec![instruction!(load_fast "self")]),
        Expr::Callable(CallableContents::ToSelf { arguments, .. }) => {
            let callable = Callable::new_recursive_call(arguments);

            callable.compile(state)
        }
        Expr::ReferenceToConstructor(class_type) => {
            let id = class_type.abs_id();
            Ok(vec![instruction!(load_self_export id)])
        }
        Expr::Index { lhs_raw, index } => {
            let mut result = lhs_raw.compile(state)?;
            result.append(&mut index.compile(state)?);
            Ok(result)
        }
        Expr::DotLookup { lhs, dot_chain, .. } => {
            let mut result = lhs.compile(state)?;
            result.append(&mut dot_chain.compile(state)?);
            Ok(result)
        }
        Expr::Nil => Ok(vec![instruction!(reserve_primitive)]),
    }
}

impl Compile for Expr {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>> {
        compile_depth(self, state, state.poll_temporary_register())
    }
}

fn parse_expr(
    pairs: Pairs<Rule>,
    user_data: Rc<AssocFileData>,
) -> Result<Expr, Vec<anyhow::Error>> {
    log::debug!("parse_expr");
    let maybe_expr = PRATT_PARSER
        .map_primary(
            |primary| -> Result<(Expr, Option<Span>), Vec<anyhow::Error>> {
                let primary_span = primary.as_span();
                let expr: Expr = match primary.as_rule() {
                    Rule::number => {
                        let raw_string = primary.as_str();
                        let child = primary.into_inner().next().unwrap();
                        let number = map_err(number::number_from_string(raw_string, child.as_rule()), primary_span, &user_data.get_file_name(), "MScript does not recognize this number".to_owned()).to_err_vec()?;

                        Expr::Value(Value::Number(number))
                    }
                    Rule::nil => Expr::Nil,
                    Rule::string => {
                        let ast_string = Parser::string(Node::new_with_user_data(primary, user_data.clone())).to_err_vec()?;
                        Expr::Value(Value::String(ast_string))
                    }
                    Rule::ident => {
                        let raw_string = primary.as_str();

                        if raw_string == "self" {
                            return Ok((Expr::ReferenceToSelf, Some(primary_span)))
                        }

                        if raw_string == "Self" {
                            let class = user_data
                                .get_type_of_executing_class()
                                .details(primary_span, &user_data.get_source_file_name(), "`Self` refers to the constructor of a class, but it is only valid inside the body of said class.")
                                .to_err_vec()?;

                            let class_ty = class.clone();

                            return Ok((Expr::ReferenceToConstructor(class_ty), Some(primary_span)))
                        }

                        let file_name = user_data.get_file_name();

                        let (ident, is_callback) = user_data
                            .get_dependency_flags_from_name(raw_string)
                            .with_context(|| {
                                new_err(
                                    primary.as_span(),
                                    &file_name,
                                    "use of undeclared variable".into(),
                                )
                            })
                            .to_err_vec()?;

                        let cloned = if is_callback {
                            ident.clone().wrap_in_callback().to_err_vec()?
                        } else {
                            ident.clone()
                        };

                        Expr::Value(Value::Ident(cloned))
                    }
                    Rule::math_expr => parse_expr(primary.into_inner(), user_data.clone())?,
                    Rule::boolean => {
                        Expr::Value(Value::Boolean(boolean_from_str(primary.as_str())))
                    }
                    rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
                };

                Ok((expr, Some(primary_span)))
            },
        )
        .map_infix(|lhs, op, rhs| {
            let (lhs, l_span) = lhs?;
            let (rhs, _) = rhs?;

            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::subtract => Op::Subtract,
                Rule::multiply => Op::Multiply,
                Rule::divide => Op::Divide,
                Rule::modulo => Op::Modulo,
                Rule::lt => Op::Lt,
                Rule::lte => Op::Lte,
                Rule::gt => Op::Gt,
                Rule::gte => Op::Gte,
                Rule::eq => Op::Eq,
                Rule::neq => Op::Neq,
                Rule::and => Op::And,
                Rule::or => Op::Or,
                Rule::xor => Op::Xor,
                Rule::unwrap => Op::Unwrap,
                Rule::add_assign => Op::AddAssign,
                Rule::sub_assign => Op::SubAssign,
                Rule::mul_assign => Op::MulAssign,
                Rule::div_assign => Op::DivAssign,
                Rule::mod_assign => Op::ModAssign,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };

            let bin_op = Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };

            if let Err(e) = bin_op.validate() {
                return Err(vec![new_err(
                    l_span.unwrap_or_else(|| panic!("{}", e.to_string())),
                    &user_data.get_source_file_name(),
                    e.to_string(),
                )]);
            }

            Ok((bin_op, None))
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            // Rule::unary_minus if op.as_str().len() % 2 != 0 => Expr::UnaryMinus(Box::new(rhs)),
            // Rule::unary_minus => rhs,
            Rule::unary_minus => Ok((Expr::UnaryMinus(Box::new(rhs?.0)), None)),
            Rule::not => Ok((Expr::UnaryNot(Box::new(rhs?.0)), None)),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::callable => {
                let (lhs, l_span) = lhs?;

                let lhs = Box::new(lhs);

                match lhs.as_ref() {
                    Expr::ReferenceToSelf => {
                        let return_type = user_data.return_statement_expected_yield_type().map(|x| x.clone());

                        let function_arguments: Pair<Rule> = op.into_inner().next().unwrap();
                        let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));

                        let (_, parameters) = user_data
                            .get_current_executing_function()
                            .details(l_span.unwrap(), &user_data.get_source_file_name(), "`self` is not callable here".to_owned())
                            .to_err_vec()?;

                        let arguments: FunctionArguments = Parser::function_arguments(function_arguments, &parameters, None)?;


                        return Ok((Expr::Callable(CallableContents::ToSelf { return_type, arguments }), None))
                    }
                    Expr::ReferenceToConstructor(ref function_type) => {
                        let function_arguments: Pair<Rule> = op.into_inner().next().unwrap();
                        let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));

                        let function_type = function_type.constructor();

                        let parameters = function_type.parameters();

                        let arguments: FunctionArguments = Parser::function_arguments(function_arguments, parameters, None)?;

                        // let function_type = function_type.clone();

                        return Ok((Expr::Callable(CallableContents::Standard { lhs_raw: lhs, function: function_type, arguments }), None))
                    }
                    _ => ()
                }

                let lhs_ty = lhs.for_type().to_err_vec()?;

                let Some(function_type) = lhs_ty.get_function() else {
                    return Err(vec![new_err(l_span.unwrap(), &user_data.get_source_file_name(), "this is not callable".to_owned())]);
                };

                let function_arguments: Pair<Rule> = op.into_inner().next().unwrap();
                let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));
                let function_arguments: FunctionArguments = Parser::function_arguments(function_arguments, function_type.parameters(), None)?;

                Ok((Expr::Callable(CallableContents::Standard { lhs_raw: lhs, function: function_type, arguments: function_arguments }), None))
            },
            Rule::list_index => {
                let lhs = lhs?.0;

                let lhs_ty = lhs.for_type().to_err_vec()?;

                let index: Pair<Rule> = op;
                let index: Node = Node::new_with_user_data(index, Rc::clone(&user_data));
                let index: Index = Parser::list_index(index, lhs_ty)?;

                // let index_ty = index.for_type().to_err_vec()?;

                // if !types_supported_for_index.contains(&index_ty) {
                //     return Err(vec![new_err(l_span.unwrap(), &user_data.get_source_file_name(), format!("`{lhs_ty}` is not indexable with `{index_ty}` (Hint: this type supports indexes of {types_supported_for_index})"))]);
                // }

                Ok((Expr::Index { lhs_raw: Box::new(lhs), index }, None))
            },
            Rule::dot_chain => {
                let lhs = lhs?.0;

                let lhs_ty = lhs.for_type().to_err_vec()?;
                let lhs_ty = lhs_ty.assume_type_of_self(&user_data);

                let index: Pair<Rule> = op;
                let index: Node = Node::new_with_user_data(index, Rc::clone(&user_data));
                let (dot_chain, final_output_type) = Parser::dot_chain(index, &lhs_ty)?;

                let expected_type = if final_output_type.is_class_self() {
                    lhs_ty
                } else {
                    final_output_type.to_owned()
                };

                Ok((Expr::DotLookup { lhs: Box::new(lhs), dot_chain, expected_type }, None))
            }
            _ => unreachable!(),
        })
        .parse(pairs);

    maybe_expr.map(|(expr, ..)| expr)
}
