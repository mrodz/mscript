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

use std::rc::Rc;

use anyhow::{anyhow, bail, Context, Result};
use once_cell::sync::Lazy;
use pest::{iterators::Pairs, pratt_parser::PrattParser, Span};

use crate::{
    ast::number,
    instruction,
    parser::{util, AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    boolean::boolean_from_str, new_err, r#type::IntoType, string::AstString, Compile, CompiledItem,
    Dependencies, Dependency, Value,
};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    use pest::pratt_parser::{Assoc::*, Op};
    use Rule::*;

    PrattParser::new()
        .op(Op::infix(add, Left) | Op::infix(subtract, Left))
        .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
        .op(Op::prefix(unary_minus))
        .op(Op::infix(lt, Left)
            | Op::infix(lte, Left)
            | Op::infix(gt, Left)
            | Op::infix(gte, Left)
            | Op::infix(eq, Left)
            | Op::infix(neq, Left))
});

#[derive(Debug, Clone)]
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
}

impl Op {
    pub fn symbol(&self) -> &'static str {
        use Op::*;
        match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            Modulo => "%",
            Lt => "<",
            Lte => "<=",
            Gt => ">",
            Gte => ">=",
            Eq => "==",
            Neq => "!=",
        }
    }
}

#[derive(Debug)]
pub(crate) enum Expr {
    // Number(Number),
    Value(Value),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

impl Expr {
    pub fn validate(&self) -> Result<()> {
        self.for_type().map(|_| ())
    }
}

impl IntoType for Expr {
    fn for_type(&self) -> Result<super::TypeLayout> {
        match self {
            Expr::Value(val) => val.for_type(),
            Expr::BinOp { lhs, op, rhs } => {
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
            Expr::UnaryMinus(val) => val.for_type(),
        }
    }
}

impl Dependencies for Expr {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Value(val) => val.net_dependencies(),
            Self::UnaryMinus(expr) => expr.net_dependencies(),
            Self::BinOp { lhs, rhs, .. } => {
                let mut lhs_dep = lhs.net_dependencies();
                let mut rhs_dep = rhs.net_dependencies();

                lhs_dep.append(&mut rhs_dep);

                lhs_dep
            }
        }
    }
}

static mut EXPR_REGISTER: usize = 0;

#[derive(Debug)]
struct ExprRegister {
    register: Option<usize>,
}

impl ExprRegister {
    pub fn new() -> Self {
        let mut result = Self { register: None };

        result.reserve_register();

        result
    }

    fn reserve_register(&mut self) {
        unsafe {
            self.register = Some(EXPR_REGISTER);
            EXPR_REGISTER += 1;
        }
    }

    pub fn repr(&mut self) -> String {
        format!("#{}", self.register.unwrap())
    }
}

impl Drop for ExprRegister {
    fn drop(&mut self) {
        unsafe {
            if let Some(register) = self.register {
                if register == EXPR_REGISTER - 1 {
                    EXPR_REGISTER -= 1;
                } else {
                    unreachable!("dropped out of order")
                }
            }
        }
    }
}

fn compile_depth(
    expr: &Expr,
    function_buffer: &mut Vec<CompiledItem>,
    mut depth: ExprRegister,
) -> Result<Vec<CompiledItem>> {
    match expr {
        Expr::Value(val) => val.compile(function_buffer),
        Expr::UnaryMinus(expr) => {
            if let Expr::Value(value) = expr.as_ref() {
                match value {
                    Value::Ident(ident) => {
                        let x = ident.ty().context("no type data")?;
                        if !x.can_negate() {
                            bail!("cannot negate")
                        }
                    }
                    Value::Number(..) => (),
                    _ => bail!("cannot negate"),
                }
            }

            let mut eval = expr.compile(function_buffer)?;

            eval.push(instruction!(neg));

            Ok(eval)
        }
        Expr::BinOp { lhs, op, rhs } => {
            let mut lhs = compile_depth(lhs, function_buffer, ExprRegister::new())?;
            let mut rhs = compile_depth(rhs, function_buffer, ExprRegister::new())?;

            let temp_name = depth.repr();

            // store the initialization to the "second part"
            rhs.push(instruction!(store temp_name));

            // init "first part" of the bin_op
            rhs.append(&mut lhs);

            // load the "second part" of the bin_op, even though we already calculated it
            rhs.push(instruction!(load temp_name));

            match op {
                Op::Eq => rhs.push(instruction!(equ)),
                Op::Neq => rhs.push(instruction!(neq)),
                _ => {
                    let symbol = op.symbol();
                    rhs.push(instruction!(bin_op symbol))
                }
            }

            Ok(rhs)
        }
    }
}

impl Compile for Expr {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        compile_depth(self, function_buffer, ExprRegister::new())
    }
}

pub(crate) fn parse_expr(
    pairs: Pairs<Rule>,
    user_data: Rc<AssocFileData>,
) -> Result<Expr, Vec<anyhow::Error>> {
    let maybe_expr = PRATT_PARSER
        .map_primary(
            |primary| -> Result<(Expr, Option<Span>), Vec<anyhow::Error>> {
                let primary_span = primary.as_span();
                let expr: Expr = match primary.as_rule() {
                    Rule::number => {
                        let raw_string = primary.as_str();
                        let child = primary.into_inner().next().unwrap();
                        let number =
                            number::number_from_string(raw_string, child.as_rule()).unwrap();

                        Expr::Value(Value::Number(number))
                    }
                    Rule::string => {
                        let raw_string = primary.as_str();
                        Expr::Value(Value::String(AstString::Plain(raw_string.to_owned())))
                    }
                    Rule::ident => {
                        let raw_string = primary.as_str();

                        let file_name = user_data.get_file_name();

                        let (ident, is_callback) = user_data
                            .get_dependency_flags_from_name(&raw_string.to_string())
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
                    Rule::callable => {
                        let x = util::parse_with_userdata(
                            Rule::callable,
                            primary.as_str(),
                            user_data.clone(),
                        )
                        .map_err(|e| vec![anyhow!(e)])?;

                        let single = x.single().map_err(|e| vec![anyhow!(e)])?;

                        let callable = Parser::callable(single)?;

                        Expr::Value(Value::Callable(callable))
                    }
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
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };

            let bin_op = Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };

            if let Err(e) = bin_op.validate() {
                return Err(vec![new_err(
                    l_span.unwrap(),
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
            _ => unreachable!(),
        })
        .parse(pairs);

    maybe_expr.map(|(expr, ..)| expr)
}

impl Parser {
    pub fn math_expr(input: Node) -> Result<Expr, Vec<anyhow::Error>> {
        let children_as_pairs = input.children().into_pairs();
        let parsed = parse_expr(children_as_pairs, input.user_data().clone());

        parsed
    }
}
