use std::fmt::Debug;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
    VecErr,
};

use super::{
    Assignment, Break, Compile, CompiledItem, Continue, Dependencies, Dependency, Expr,
    IfStatement, NumberLoop, PrintStatement, Reassignment, ReturnStatement, WhileLoop, Assertion,
};

#[derive(Debug)]
pub(crate) enum Declaration {
    Assignment(Assignment),
    Reassignment(Reassignment),
    Expr(Expr),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    NumberLoop(NumberLoop),
    Continue(Continue),
    Break(Break),
    Assertion(Assertion),
}

impl Dependencies for Declaration {
    fn supplies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.supplies(),
            Self::Expr(expr) => expr.supplies(),
            Self::PrintStatement(print_statement) => print_statement.supplies(),
            Self::ReturnStatement(return_statement) => return_statement.supplies(),
            Self::IfStatement(if_statement) => if_statement.supplies(),
            Self::WhileLoop(while_loop) => while_loop.supplies(),
            Self::NumberLoop(number_loop) => number_loop.supplies(),
            Self::Assertion(assertion) => assertion.supplies(),
            Self::Reassignment(_) | Self::Continue(_) | Self::Break(_) => vec![],
        }
    }

    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.net_dependencies(),
            Self::Expr(expr) => expr.net_dependencies(),
            Self::PrintStatement(print_statement) => print_statement.net_dependencies(),
            Self::ReturnStatement(return_statement) => return_statement.net_dependencies(),
            Self::IfStatement(if_statement) => if_statement.net_dependencies(),
            Self::WhileLoop(while_loop) => while_loop.net_dependencies(),
            Self::NumberLoop(number_loop) => number_loop.net_dependencies(),
            Self::Reassignment(reassignment) => reassignment.net_dependencies(),
            Self::Assertion(assertion) => assertion.net_dependencies(),
            Self::Continue(_) | Self::Break(_) => vec![],
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::PrintStatement(x) => x.compile(function_buffer),
            Self::Expr(x) => {
                let mut expr_compiled = x.compile(function_buffer)?;
                expr_compiled.push(instruction!(void));
                Ok(expr_compiled)
            }
            Self::Assignment(x) => x.compile(function_buffer),
            Self::Reassignment(x) => x.compile(function_buffer),
            Self::ReturnStatement(x) => x.compile(function_buffer),
            Self::IfStatement(x) => x.compile(function_buffer),
            Self::WhileLoop(x) => x.compile(function_buffer),
            Self::Continue(x) => x.compile(function_buffer),
            Self::Break(x) => x.compile(function_buffer),
            Self::NumberLoop(x) => x.compile(function_buffer),
            Self::Assertion(x) => x.compile(function_buffer),
        }
    }
}

impl Parser {
    pub fn declaration(input: Node) -> Result<Declaration, Vec<anyhow::Error>> {
        let declaration = input.children().next().unwrap();

        let matched = match declaration.as_rule() {
            Rule::assignment => Declaration::Assignment(Self::assignment(declaration)?),
            // Rule::callable => Declaration::Expr(math_expr(declaration)?),
            Rule::print_statement => {
                let print_stmt = Self::print_statement(declaration);
                Declaration::PrintStatement(print_stmt?)
            }
            Rule::return_statement => {
                Declaration::ReturnStatement(Self::return_statement(declaration)?)
            }
            Rule::if_statement => Declaration::IfStatement(Self::if_statement(declaration)?),
            Rule::while_loop => Declaration::WhileLoop(Self::while_loop(declaration)?),
            Rule::continue_statement => {
                Declaration::Continue(Self::continue_statement(declaration).to_err_vec()?)
            }
            Rule::break_statement => Declaration::Break(Self::break_statement(input).to_err_vec()?),
            Rule::number_loop => Declaration::NumberLoop(Self::number_loop(declaration)?),
            Rule::math_expr => Declaration::Expr(Expr::parse(input)?),
            Rule::reassignment => Declaration::Reassignment(Self::reassignment(declaration)?),
            Rule::assertion => Declaration::Assertion(Self::assertion(declaration)?),
            x => unreachable!("{x:?} is not supported"),
        };

        Ok(matched)
    }
}
