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

use anyhow::Result;
use once_cell::sync::Lazy;
use pest::{pratt_parser::PrattParser, iterators::Pairs};

use crate::{parser::{Parser, Node, Rule}, ast::number};

use super::{Value, Number};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
	use pest::pratt_parser::{Assoc::*, Op};
	use Rule::*;

	PrattParser::new()
		.op(Op::infix(add, Left) | Op::infix(subtract, Left))
		.op(Op::infix(multiply, Left) | Op::infix(divide, Left) | Op::infix(modulo, Left))
		.op(Op::prefix(unary_minus))
});

#[derive(Debug)]
pub enum Op {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug)]
pub enum Expr {
    Number(Number),
    UnaryMinus(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: Op,
        rhs: Box<Expr>,
    },
}

pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
	PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Expr::Number(number::number_from_string(primary.as_str(), primary.into_inner().next().unwrap().as_rule()).unwrap()),
            Rule::math_expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
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
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::unary_minus => Expr::UnaryMinus(Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(pairs)
}

impl Parser {
	// pub fn math_expr(l: )
	pub fn math_expr(input: Node) -> Result<Value> {
		// let input = input;
		let x = input.children().into_pairs();

		let y = parse_expr(x);
		dbg!(y);

		todo!()
	}
}