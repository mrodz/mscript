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

use std::borrow::Cow;

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::{
    ast::number,
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
};

use super::{new_err, r#type::IntoType, string::AstString, Compile, Dependencies, Value};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    use pest::pratt_parser::{Assoc::*, Op};
    use Rule::*;

    PrattParser::new()
        .op(Op::infix(add, Left) | Op::infix(subtract, Left))
        .op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
        .op(Op::prefix(unary_minus))
});

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

impl Op {
    pub fn symbol(&self) -> char {
        use Op::*;
        match self {
            Add => '+',
            Subtract => '-',
            Multiply => '*',
            Divide => '/',
            Modulo => '%',
        }
    }
}

#[derive(Debug, Clone)]
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
        self.into_type().map(|_| ())
    }
}

impl IntoType for Expr {
    fn into_type(&self) -> Result<super::TypeLayout> {
        match self {
            Expr::Value(val) => val.into_type(),
            Expr::BinOp { lhs, op, rhs } => {
                let lhs = lhs.into_type()?;
                let rhs = rhs.into_type()?;

                lhs.get_output_type(&rhs, op).with_context(|| {
                    format!(
                        "invalid operation: {} {} {rhs}",
                        lhs.get_type_recursively(),
                        op.symbol()
                    )
                })
            }
            Expr::UnaryMinus(val) => val.into_type(),
        }
    }
}

impl Dependencies for Expr {
    fn get_dependencies(&self) -> Option<Box<[super::Dependency]>> {
        match self {
            Self::Value(val) => val.get_dependencies(),
            Self::UnaryMinus(expr) => expr.get_dependencies(),
            Self::BinOp { lhs, rhs, .. } => {
                let lhs_dep = lhs.get_dependencies();
                let rhs_dep = rhs.get_dependencies();
                if let Some(lhs) = lhs_dep {
                    if let Some(rhs) = rhs_dep {
                        let mut vec = lhs.into_vec();
                        vec.append(&mut rhs.into_vec());
                        Some(vec.into_boxed_slice())
                    } else {
                        Some(lhs)
                    }
                } else {
                    rhs.get_dependencies()
                }
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

fn compile_depth(expr: &Expr, mut depth: ExprRegister) -> Result<Vec<super::CompiledItem>> {
    match expr {
        Expr::Value(val) => val.compile(),
        Expr::UnaryMinus(expr) => {
            if let box Expr::Value(value) = expr {
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

            let mut eval = expr.compile()?;

            eval.push(instruction!(neg));

            Ok(eval)
        }
        Expr::BinOp { lhs, op, rhs } => {
            let mut lhs = compile_depth(&lhs, ExprRegister::new())?;
            let mut rhs = compile_depth(&rhs, ExprRegister::new())?;

            let temp_name = depth.repr();

            // store the initialization to the "second part"
            rhs.push(instruction!(store temp_name));

            // init "first part" of the bin_op
            rhs.append(&mut lhs);

            // load the "second part" of the bin_op, even though we already calculated it
            rhs.push(instruction!(load temp_name));

            let symbol = op.symbol();

            rhs.push(instruction!(bin_op symbol));

            Ok(rhs)
        }
    }
}

impl Compile for Expr {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        compile_depth(self, ExprRegister::new())
    }
}

pub(crate) fn parse_expr(pairs: Pairs<Rule>, user_data: &AssocFileData) -> Result<Expr> {
    PRATT_PARSER
        .map_primary(|primary| -> Result<Expr> {
            match primary.as_rule() {
                Rule::number => {
                    let raw_string = primary.as_str();
                    let child = primary.into_inner().next().unwrap();
                    let number = number::number_from_string(raw_string, child.as_rule()).unwrap();

                    Ok(Expr::Value(Value::Number(number)))
                }
                Rule::string => {
                    let raw_string = primary.as_str();
                    Ok(Expr::Value(Value::String(AstString::Plain(
                        raw_string.to_owned(),
                    ))))
                }
                Rule::ident => {
                    let raw_string = primary.as_str();

                    let file_name = user_data.get_file_name();

                    let file_name: Cow<String> = if file_name.ends_with(".mmm") {
                        let mut owned = file_name[..file_name.len() - 3].to_owned();
                        owned.push_str("ms");
                        Cow::Owned(owned)
                    } else {
                        Cow::Borrowed(&*file_name)
                    };

                    let ty = user_data
                        .get_dependency_flags_from_name(raw_string.to_string())
                        .with_context(|| {
                            new_err(
                                primary.as_span(),
                                &file_name,
                                "use of undeclared variable".into(),
                            )
                        })?;
                    Ok(Expr::Value(Value::Ident(ty.0.clone())))
                }
                Rule::math_expr => parse_expr(primary.into_inner(), user_data),
                rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => Op::Add,
                Rule::subtract => Op::Subtract,
                Rule::multiply => Op::Multiply,
                Rule::divide => Op::Divide,
                Rule::modulo => Op::Modulo,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            Ok(Expr::BinOp {
                lhs: Box::new(lhs?),
                op,
                rhs: Box::new(rhs?),
            })
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            // Rule::unary_minus if op.as_str().len() % 2 != 0 => Expr::UnaryMinus(Box::new(rhs)),
            // Rule::unary_minus => rhs,
            Rule::unary_minus => Ok(Expr::UnaryMinus(Box::new(rhs?))),
            _ => unreachable!(),
        })
        .parse(pairs)
}

impl Parser {
    // pub fn math_expr(l: )
    pub fn math_expr(input: Node) -> Result<Expr> {
        let children_as_pairs = input.children().into_pairs();

        let token_tree = parse_expr(children_as_pairs, input.user_data());

        token_tree
    }
}
