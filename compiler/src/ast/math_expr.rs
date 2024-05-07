use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
};

use crate::{
    ast::{number, r#type::TypecheckFlags, Callable, ConstexprEvaluation, Ident},
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{
    dot_lookup::DotChain, function::FunctionType, list::Index, map_err, new_err, r#type::IntoType,
    string::AstString, ClassType, CompilationState, Compile, CompileTimeEvaluate, CompiledItem,
    Dependencies, Dependency, FunctionArguments, NativeType, StrWrapper, TemporaryRegister,
    TypeLayout, Value,
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
        .op(infix!(is))
        .op(infix!(unwrap))
        .op(Op::prefix(optional_unwrap))
        .op(infix!(add_assign)
            | infix!(sub_assign)
            | infix!(mul_assign)
            | infix!(div_assign)
            | infix!(mod_assign))
        .op(infix!(or) | infix!(xor))
        .op(infix!(and))
        .op(infix!(lt) | infix!(lte) | infix!(gt) | infix!(gte) | infix!(eq) | infix!(neq))
        .op(infix!(binary_or) | infix!(binary_and))
        .op(infix!(binary_xor))
        .op(infix!(bitwise_ls) | infix!(bitwise_rs))
        .op(infix!(add) | infix!(subtract))
        .op(infix!(multiply) | infix!(divide) | infix!(modulo))
        .op(Op::prefix(not) | Op::prefix(unary_minus) | Op::prefix(r#typeof))
        .op(Op::postfix(dot_chain)
            | Op::postfix(list_index)
            | Op::postfix(callable)
            | Op::postfix(optional_or))
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
    BinaryXor,
    BinaryOr,
    BinaryAnd,
    BitwiseLs,
    BitwiseRs,
    Is,
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
            Op::BinaryXor => "xor",
            Op::BinaryAnd => "&",
            Op::BinaryOr => "|",
            Op::BitwiseLs => "<<",
            Op::BitwiseRs => ">>",
            Op::Is => "is",
        }
    }

    pub const fn is_op_assign(&self) -> bool {
        use Op::*;
        matches!(
            self,
            AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
        )
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

pub(crate) trait UnwrapSpanDisplayable: Debug + Display {}
impl<T> UnwrapSpanDisplayable for T where T: Debug + Display {}

#[derive(Debug, Clone)]
pub enum ReferenceToSelf {
    Class(ClassType),
    Function,
    Invalid,
}

#[derive(Debug)]
pub(crate) enum Expr {
    Nil,
    Value(Value),
    ReferenceToSelf(RefCell<ReferenceToSelf>),
    ReferenceToConstructor(ClassType),
    UnaryMinus(Box<Expr>),
    UnaryNot(Box<Expr>),
    Typeof(Box<Expr>, String),
    UnaryUnwrap {
        value: Box<Expr>,
        span: Box<dyn UnwrapSpanDisplayable>,
    },
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
    NilEval {
        primary: Box<Expr>,
        fallback: Value,
    },
}

impl Expr {
    pub(crate) fn validate(
        &self,
        flags: &TypecheckFlags<impl Deref<Target = ClassType> + Debug>,
    ) -> Result<&Self> {
        self.for_type(flags).map(|_| self)
    }

    pub(crate) fn validate_owned(
        self,
        flags: &TypecheckFlags<impl Deref<Target = ClassType> + Debug>,
    ) -> Result<Self> {
        self.for_type(flags).map(|_| self)
    }

    pub(crate) fn parse(input: Node) -> Result<Expr, Vec<anyhow::Error>> {
        let children_as_pairs = input.children().into_pairs();
        parse_expr(children_as_pairs, input.user_data().clone())
    }

    pub(crate) fn for_type(
        &self,
        flags: &TypecheckFlags<impl Deref<Target = ClassType> + Debug>,
    ) -> Result<super::TypeLayout> {
        match self {
            Expr::Value(val) => val.for_type(flags),
            Expr::BinOp { lhs, op, rhs } => {
                let lhs = if op.is_op_assign() {
                    match lhs.as_ref() {
                        Expr::Value(Value::Ident(ident)) => {
                            if ident.is_const() {
                                bail!(
                                    "cannot reassign using {op} to {}, which is const",
                                    ident.name()
                                )
                            }
                            Cow::Owned(lhs.for_type(flags)?)
                        }
                        Expr::DotLookup { expected_type, .. } => Cow::Borrowed(expected_type),
                        _ => bail!("invalid left operand for {op}"),
                    }
                } else {
                    Cow::Owned(lhs.for_type(flags)?)
                };

                let rhs = rhs.for_type(flags)?;

                lhs.get_output_type(&rhs, op, flags)
                    .with_context(|| format!("invalid operation: {} {} {}", lhs, op.symbol(), rhs))
            }
            Expr::UnaryMinus(val) | Expr::UnaryNot(val) => val.for_type(flags),
            Expr::Callable(CallableContents::Standard { function, .. }) => {
                let return_type = function.return_type();

                let return_type = return_type.get_type().context(VOID_ERROR_MESSAGE)?;

                Ok(return_type.clone().into_owned())
            }
            Expr::Callable(CallableContents::ToSelf { return_type, .. }) => {
                let Some(return_type) = return_type else {
                    bail!(VOID_ERROR_MESSAGE)
                };

                Ok(return_type.clone().into_owned())
            }
            Expr::ReferenceToSelf(is_valid) => {
                if let ReferenceToSelf::Class(class_type) = &*is_valid.borrow() {
                    Ok(TypeLayout::ClassSelf(Some(class_type.clone())))
                } else {
                    bail!("this reference to `self` is not valid here")
                }
            }
            Expr::ReferenceToConstructor(class_type) => {
                Ok(TypeLayout::Function(class_type.constructor()))
            }
            Expr::Index { index, .. } => index.for_type(),
            Expr::DotLookup { expected_type, .. } => Ok(expected_type.to_owned()),
            Expr::Nil => Ok(TypeLayout::Optional(None)),
            Expr::UnaryUnwrap { value, span: _ } => {
                let ty = value.for_type(flags)?;

                if let (true, Some(ty)) = ty.is_optional() {
                    return Ok(ty.clone().into_owned());
                }

                Ok(ty)
            }
            Expr::NilEval { primary, fallback } => {
                let primary = primary.for_type(flags)?;
                let fallback = fallback.for_type(flags)?;
                Ok(if let (true, ty) = primary.is_optional() {
                    if let Some(ty) = ty {
                        let ty = ty.disregard_distractors(false);

                        if ty.is_optional().1.is_some() && fallback.is_optional().1.is_some() {
                            // only check if neither of the operands is `nil`
                            assert_eq!(ty, &fallback);
                        }

                        ty.clone()
                    } else {
                        // `lhs` is nil, use fallback to infer type
                        fallback
                    }
                } else {
                    assert_eq!(
                        primary.disregard_distractors(false),
                        fallback.disregard_distractors(false)
                    );
                    primary
                })
            }
            Expr::Typeof(_, repr) => Ok(TypeLayout::Native(NativeType::Str(StrWrapper::sized(
                repr.len(),
            )))),
        }
    }
}

impl CompileTimeEvaluate for Expr {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        // self.validate()?;

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

                if constexpr_eval
                    .for_type(&TypecheckFlags::<&ClassType>::classless())?
                    .supports_negate()
                {
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
                    Op::Subtract => lhs - rhs,
                    Op::Multiply => lhs * rhs,
                    Op::Divide => lhs / rhs,
                    Op::Modulo => lhs % rhs,
                    Op::BitwiseLs => lhs << rhs,
                    Op::BitwiseRs => lhs >> rhs,
                    Op::BinaryAnd => lhs & rhs,
                    Op::BinaryOr => lhs | rhs,
                    Op::BinaryXor => lhs ^ rhs,
                    _ => return Ok(ConstexprEvaluation::Impossible),
                };

                Ok(ConstexprEvaluation::Owned(Value::Number(bin_op_applied?)))
            }
            Self::ReferenceToSelf { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::ReferenceToConstructor(..) => Ok(ConstexprEvaluation::Impossible),
            Self::Nil => Ok(ConstexprEvaluation::Owned(Value::nil())),
            Self::Callable { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::Index { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::DotLookup { .. } => Ok(ConstexprEvaluation::Impossible),
            Self::UnaryUnwrap { value, .. } => {
                let ConstexprEvaluation::Owned(value) = value.try_constexpr_eval()? else {
                    return Ok(ConstexprEvaluation::Impossible);
                };

                if value.is_nil() {
                    bail!("at compile time, this operation was flagged because it always unwraps `nil`")
                }

                Ok(ConstexprEvaluation::Owned(value))
            }
            Self::NilEval { primary, fallback } => {
                let ConstexprEvaluation::Owned(value) = primary.try_constexpr_eval()? else {
                    return Ok(ConstexprEvaluation::Impossible);
                };

                if value.is_nil() {
                    fallback.try_constexpr_eval()
                } else {
                    Ok(ConstexprEvaluation::Owned(value))
                }
            }
            Self::Typeof(_, repr) => Ok(ConstexprEvaluation::Owned(Value::String(
                AstString::Plain(repr.clone()),
            ))),
        }
    }
}

static VOID_ERROR_MESSAGE: &str = "this scope does not yield a value";

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
            E::DotLookup { lhs, .. } => {
                let x = lhs.net_dependencies();

                println!("{x:?}");

                x
            }
            E::ReferenceToSelf(reference_to_self) => {
                let reference_to_self = match reference_to_self.borrow().clone() {
                    ReferenceToSelf::Class(class) => TypeLayout::Class(class.clone()),
                    _ => {
                        log::warn!("Invalid reference to self");
                        return vec![];
                    }
                };
                let self_dependency = Dependency::new(Cow::Owned(Ident::new(
                    "self".to_owned(),
                    Some(Cow::Owned(reference_to_self)),
                    false,
                )));
                vec![self_dependency]
            }
            E::ReferenceToConstructor(..) => vec![],
            E::Nil => vec![],
            E::UnaryUnwrap { value, .. } => value.net_dependencies(),
            E::NilEval { primary, fallback } => {
                let mut primary = primary.net_dependencies();
                primary.append(&mut fallback.net_dependencies());
                primary
            }
            E::Typeof(val, _) => val.net_dependencies(),
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
            // This should not be in the `compile` function!!!!
            // if let Expr::Value(value) = expr.as_ref() {
            //     let ty = value.for_type(flags)?;
            //     if !ty.is_boolean() {
            //         bail!("cannot apply binary not to {:?}", ty)
            //     }
            // }

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
        Expr::BinOp {
            lhs: lhs_raw,
            op,
            rhs,
        } => {
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
                    match lhs_raw.as_ref() {
                        Expr::Value(Value::Ident(ident)) => {
                            rhs.push(instruction!(bin_op_assign (op.symbol()) (ident.name())));

                            return Ok(rhs)
                        }
                        Expr::DotLookup { .. } => {
                            rhs.push(instruction!(store_fast depth));
                            rhs.append(&mut lhs);
                            rhs.push(instruction!(load_fast depth));
                            rhs.push(instruction!(bin_op_assign (op.symbol())));

                            return Ok(rhs)

                        }
                        lhs => unimplemented!("bin_op_asign has not been implemented for this left hand operand: {lhs:?}")
                    }
                }
                Op::Unwrap => {
                    let Expr::Value(Value::Ident(ident)) = lhs_raw.as_ref() else {
                        unreachable!("Expected ident in lhs, but got {lhs_raw:#?}");
                    };

                    let name = ident.name();

                    rhs.push(instruction!(unwrap_into name));

                    return Ok(rhs);
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
        Expr::ReferenceToSelf { .. } => Ok(vec![instruction!(load_fast "self")]),
        Expr::Callable(CallableContents::ToSelf { arguments, .. }) => {
            let callable = Callable::new_recursive_call(arguments);

            callable.compile(state)
        }
        Expr::ReferenceToConstructor(class_type) => {
            let id = class_type.name();
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
        Expr::NilEval { primary, fallback } => {
            let mut value_compiled = primary.compile(state)?;

            let mut fallback_compiled = fallback.compile(state)?;

            let instructions_to_skip = fallback_compiled.len() + 1;

            value_compiled.push(instruction!(jmp_not_nil instructions_to_skip));

            value_compiled.append(&mut fallback_compiled);

            Ok(value_compiled)
        }
        Expr::UnaryUnwrap { value, span } => {
            let mut value_compiled = value.compile(state)?;

            value_compiled.push(instruction!(unwrap span));

            Ok(value_compiled)
        }
        Expr::Typeof(_, name) => Ok(vec![instruction!(make_str name)]),
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
    let maybe_expr = PRATT_PARSER
        .map_primary(
            |primary_pair| -> Result<(Expr, Option<Pair<'_, Rule>>), Vec<anyhow::Error>> {
                let primary = Node::new_with_user_data(primary_pair.clone(), user_data.clone());

                let primary_span = primary.as_span();
                let expr: Expr = match primary.as_rule() {
                    Rule::number => {
                        let raw_string = primary.as_str();
                        let child = primary.children().single().unwrap();
                        let number = map_err(number::number_from_string(raw_string, child.as_rule()), primary_span, &user_data.get_file_name(), "MScript does not recognize this number".to_owned()).to_err_vec()?;

                        Expr::Value(Value::Number(number))
                    }
                    Rule::nil => Expr::Nil,
                    Rule::string => {
                        let ast_string = Parser::string(primary).to_err_vec()?;
                        Expr::Value(Value::String(ast_string))
                    }
                    Rule::ident => {
                        let raw_string = primary.as_str();

                        match raw_string {
                            "self" => {
                                let self_reference = if let Some(c) = user_data.get_type_of_executing_class() {
                                    ReferenceToSelf::Class(c.to_owned())
                                } else {
                                    ReferenceToSelf::Invalid
                                };
                                return Ok((Expr::ReferenceToSelf(RefCell::new(self_reference)), Some(primary_pair)))
                            },
                            "Self" => {
                                let class = user_data
                                .get_type_of_executing_class()
                                .details(primary_span, &user_data.get_source_file_name(), "`Self` refers to the constructor of a class, but it is only valid inside the body of said class.")
                                .to_err_vec()?;

                                let class_ty = class.clone();

                                return Ok((Expr::ReferenceToConstructor(class_ty), Some(primary_pair)))
                            }
                            "true" => return Ok((Expr::Value(Value::Boolean(true)), Some(primary_pair))),
                            "false" => return Ok((Expr::Value(Value::Boolean(false)), Some(primary_pair))),
                            _ => ()
                        }

                        let file_name = user_data.get_source_file_name();

                        let (ident, is_callback) = user_data
                            .get_dependency_flags_from_name(raw_string)
                            .with_context(|| {
                                let hint = match primary.as_str() {
                                    "print" => "\n        + hint: printing in MScript v1 uses the `print` keyword, like\n        |\n        |   print \"Hello World\"\n        |",
                                    "null" => " (hint: an empty optional in MScript is represented by the `nil` keyword)",
                                    "True" => " (hint: a truthy boolean value in MScript is represented by the `true` keyword)",
                                    "False" => " (hint: a falsey boolean value in MScript is represented by the `false` keyword)",
                                    "or" => "\n        + hint: MScript uses whitespaces as a delimiter for expressions, so if you're trying to use `or` after a function call, wrap it in parentheses:\n        |\n        |   # bad: a.b() or 5\n        |   (a.b()) or 5\n        |",
                                    _ => ""
                                };

                                new_err(
                                    primary.as_span(),
                                    &file_name,
                                    format!("use of undeclared variable{hint}"),
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
                    Rule::function => Expr::Value(Value::Function(Parser::function(primary)?)),
                    Rule::list => Expr::Value(Value::List(Parser::list(primary)?)),
                    Rule::math_expr => parse_expr(primary_pair.clone().into_inner(), user_data.clone())?,
                    Rule::map => Expr::Value(Value::Map(Parser::map(primary)?)),
                    rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
                };

                Ok((expr, Some(primary_pair)))
            },
        )
        .map_infix(|lhs, op, rhs| {
            let (lhs, l_span) = lhs?;
            let (rhs, r_span) = rhs?;
            let span = l_span.or(r_span).unwrap();

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
                Rule::unwrap => {
                    if span.as_rule() != Rule::ident {
                        log::error!("found `{:?}` instead of `ident`", span.as_rule());

                        return Err(vec![new_err(
                            span.as_span(),
                            &user_data.get_source_file_name(),
                            format!("this operation requires a name, but it found `{:?}`", span.as_rule()),
                        )]);
                    }

                    Op::Unwrap
                },
                Rule::add_assign => Op::AddAssign,
                Rule::sub_assign => Op::SubAssign,
                Rule::mul_assign => Op::MulAssign,
                Rule::div_assign => Op::DivAssign,
                Rule::mod_assign => Op::ModAssign,
                Rule::binary_or => Op::BinaryOr,
                Rule::binary_and => Op::BinaryAnd,
                Rule::binary_xor => Op::BinaryXor,
                Rule::bitwise_ls => Op::BitwiseLs,
                Rule::bitwise_rs => Op::BitwiseRs,
                Rule::is => Op::Is,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };


            let bin_op = Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };

            if let Err(e) = bin_op.validate(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())) {
                return Err(vec![new_err(
                    span.as_span(),
                    &user_data.get_source_file_name(),
                    e.to_string(),
                )]);
            }

            Ok((bin_op, Some(span)))
        })
        .map_prefix(|op, rhs| {
            let (expr, pair) = rhs?;

                match op.as_rule() {
                    Rule::unary_minus => {
                        let ty = expr.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).to_err_vec()?;
                        if !ty.disregard_distractors(false).supports_negate() {
                            return Err(vec![new_err(pair.unwrap().as_span(), &user_data.get_source_file_name(), format!("{ty} cannot be negated"))])
                        }
                        Ok((Expr::UnaryMinus(Box::new(expr)), pair))
                    }
                    Rule::not => {
                        let ty = expr.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).to_err_vec()?;
                        if !ty.disregard_distractors(false).is_boolean() {
                            return Err(vec![new_err(pair.unwrap().as_span(), &user_data.get_source_file_name(), format!("{ty} cannot be negated"))])
                        }
                        Ok((Expr::UnaryNot(Box::new(expr)), pair))
                    }
                    Rule::optional_unwrap => {
                        let (line, col) = pair.as_ref().unwrap().line_col();
                        Ok((Expr::UnaryUnwrap {
                            value: Box::new(expr),
                            span: Box::new(format!("{}:{line}:{col}", user_data.get_source_file_name())),
                        }, pair))
                    }
                    Rule::r#typeof => {
                        match expr.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())) {
                            Ok(ty) => Ok((Expr::Typeof(Box::new(expr), ty.to_string()), pair)),
                            Err(e) => {
                                // hacky but works
                                if e.chain().next().unwrap().to_string() == VOID_ERROR_MESSAGE {
                                    Err(vec![new_err(pair.as_ref().map(Pair::as_span).unwrap(), &user_data.get_source_file_name(), "the `typeof` operator cannot evaluate a type: this function call returns void".to_string())])
                                } else {
                                    Err(vec![e])
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::callable => {
                let (lhs, l_span) = lhs?;

                let lhs = Box::new(lhs);

                match lhs.as_ref() {
                    Expr::ReferenceToSelf(is_valid) => {
                        let return_type = user_data.return_statement_expected_yield_type().map(|x| x.clone());

                        let function_arguments: Pair<Rule> = op.into_inner().next().unwrap();
                        let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));

                        let (_, parameters) = user_data
                            .get_current_executing_function()
                            .details(l_span.as_ref().unwrap().as_span(), &user_data.get_source_file_name(), "`self` is not callable here".to_owned())
                            .to_err_vec()?;

                        let arguments: FunctionArguments = Parser::function_arguments(function_arguments, &parameters, None)?;

                        *is_valid.borrow_mut() = ReferenceToSelf::Function;

                        return Ok((Expr::Callable(CallableContents::ToSelf { return_type, arguments }), l_span))
                    }
                    Expr::ReferenceToConstructor(ref function_type) => {
                        let function_arguments: Pair<Rule> = op.into_inner().next().unwrap();
                        let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));

                        let function_type = function_type.constructor();

                        let parameters = function_type.parameters();

                        let arguments: FunctionArguments = Parser::function_arguments(function_arguments, parameters, None)?;

                        return Ok((Expr::Callable(CallableContents::Standard { lhs_raw: lhs, function: function_type, arguments }), l_span))
                    }
                    _ => ()
                }

                let lhs_ty = lhs.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).details(l_span.as_ref().unwrap().as_span(), &user_data.get_source_file_name(), "bad expression 1").to_err_vec()?;

                let Some(function_type) = lhs_ty.is_callable() else {
                    return Err(vec![new_err(l_span.unwrap().as_span(), &user_data.get_source_file_name(), format!("`{lhs_ty}` is not callable"))]);
                };

                let function_arguments: Pair<Rule> = op.clone().into_inner().next().unwrap();
                let function_arguments: Node = Node::new_with_user_data(function_arguments, Rc::clone(&user_data));
                let function_arguments: FunctionArguments = Parser::function_arguments(function_arguments, function_type.parameters(), None)?;

                Ok((Expr::Callable(CallableContents::Standard { lhs_raw: lhs, function: function_type.into_owned(), arguments: function_arguments }), Some(op)))
            },
            Rule::list_index => {
                let (lhs_expr, lhs_span) = lhs?;

                let lhs_span = lhs_span.unwrap().as_span();

                let lhs_ty = lhs_expr.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).details(lhs_span, &user_data.get_source_file_name(), "bad expression 2").to_err_vec()?;

                let index: Node = Node::new_with_user_data(op.clone(), Rc::clone(&user_data));
                let index: Index = Parser::list_index(index, lhs_ty)?;

                Ok((Expr::Index { lhs_raw: Box::new(lhs_expr), index }, Some(op)))
            },
            Rule::dot_chain => {
                let (lhs, l_span) = lhs?;
                let l_span = l_span.unwrap().as_span();

                let lhs_ty = lhs.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).details(l_span, &user_data.get_source_file_name(), "bad expression 3").to_err_vec()?;
                let lhs_ty = lhs_ty.assume_type_of_self(&user_data);

                let index: Node = Node::new_with_user_data(op.clone(), Rc::clone(&user_data));
                let (dot_chain, final_output_type) = Parser::dot_chain(index, Cow::Borrowed(&lhs_ty))?;

                let expected_type = if final_output_type.is_class_self() {
                    lhs_ty
                } else {
                    final_output_type.into_owned()
                };

                Ok((Expr::DotLookup { lhs: Box::new(lhs), dot_chain, expected_type }, Some(op)))
            }
            Rule::optional_or => {
                let (lhs, l_span) = lhs?;
                let l_span = l_span.unwrap();

                let lhs_ty = lhs.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).details(l_span.as_span(), &user_data.get_source_file_name(), "the type of this value cannot be used in an unwrap").to_err_vec()?;

                let fallback = op.into_inner().next().unwrap();
                assert_eq!(fallback.as_rule(), Rule::value);

                let value_node = Node::new_with_user_data(fallback, Rc::clone(&user_data));

                let value_span = value_node.as_span();

                let fallback = Parser::value(value_node)?;

                let fallback_ty = fallback.for_type(&TypecheckFlags::use_class(user_data.get_type_of_executing_class())).details(value_span, &user_data.get_source_file_name(), format!("this value cannot be used as a fallback for `{lhs_ty}`")).to_err_vec()?;

                let lhs_ty_for_comp = lhs_ty.disregard_optional().unwrap_or(&TypeLayout::Optional(None));

                log::debug!("`or` stmt: fallback:{fallback_ty:?} value:{lhs_ty_for_comp:?}");

                if !lhs_ty_for_comp.eq_complex(&fallback_ty, &TypecheckFlags::use_class(user_data.get_type_of_executing_class())) {
                    let ty = if let (true, ty) = lhs_ty.is_optional() {
                        ty.map(Cow::Borrowed)
                    } else {
                        Some(Cow::Owned(Cow::Owned(lhs_ty)))
                    };

                    if let Some(ty) = ty {
                        let maybe_error_hint = ty.get_error_hint_between_types(&fallback_ty, user_data.get_type_of_executing_class()).unwrap_or_default();
                        return Err(vec![new_err(value_span, &user_data.get_source_file_name(), {
                            format!("The `or` portion of this unwrap must yield `{ty}`, but `{fallback_ty}` was found{maybe_error_hint}")
                        })])
                    }

                    // else, the lhs is indeterminate and the fallback will always override it
                }

                Ok((Expr::NilEval { primary: Box::new(lhs), fallback }, Some(l_span)))
            }
            _ => unreachable!(),
        })
        .parse(pairs);

    maybe_expr.and_then(|(expr, span)| {
        expr.validate_owned(&TypecheckFlags::use_class(
            user_data.get_type_of_executing_class(),
        ))
        .details(
            span.unwrap().as_span(),
            &user_data.get_source_file_name(),
            "bad expression 4",
        )
        .to_err_vec()
    })
}
