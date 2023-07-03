use std::fmt::Debug;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule}, VecErr,
};

use super::{
    Assignment, IfStatement, ReturnStatement, Callable,
    Compile, CompiledItem, Dependencies, Dependency, PrintStatement, WhileLoop, Continue,
};

#[derive(Debug)]
pub(crate) enum Declaration {
    Assignment(Assignment),
    Callable(Callable),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    Continue(Continue),
}

impl Dependencies for Declaration {
    fn supplies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.supplies(),
            Self::Callable(callable) => callable.supplies(),
            Self::PrintStatement(print_statement) => print_statement.supplies(),
            Self::ReturnStatement(return_statement) => return_statement.supplies(),
            Self::IfStatement(if_statement) => if_statement.supplies(),
            Self::WhileLoop(while_loop) => while_loop.supplies(),
            Self::Continue(_) => vec![]
        }
    }

    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => {
                // println!("Assignment");
                assignment.net_dependencies()
            }
            Self::Callable(callable) => {
                // println!("Callable");
                callable.net_dependencies()
            }
            Self::PrintStatement(print_statement) => {
                print_statement.net_dependencies()
            }
            Self::ReturnStatement(return_statement) => return_statement.net_dependencies(),
            Self::IfStatement(if_statement) => if_statement.net_dependencies(),
            Self::WhileLoop(while_loop) => while_loop.net_dependencies(),
            Self::Continue(_) => vec![],
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::PrintStatement(x) => x.compile(function_buffer),
            Self::Callable(x) => {
                let mut callable_exe = x.compile(function_buffer)?;
                callable_exe.push(instruction!(void));
                Ok(callable_exe)
            }
            Self::Assignment(x) => x.compile(function_buffer),
            Self::ReturnStatement(x) => x.compile(function_buffer),
            Self::IfStatement(x) => x.compile(function_buffer),
            Self::WhileLoop(x) => x.compile(function_buffer),
            Self::Continue(x) => x.compile(function_buffer),
        }
    }
}

impl Parser {
    pub fn declaration(input: Node) -> Result<Declaration, Vec<anyhow::Error>> {
        let declaration = input.children().next().unwrap();

        let matched = match declaration.as_rule() {
            Rule::assignment => Declaration::Assignment(Self::assignment(declaration)?),
            Rule::callable => Declaration::Callable(Self::callable(declaration)?),
            Rule::print_statement => {
                let print_stmt = Self::print_statement(declaration);
                Declaration::PrintStatement(print_stmt?)
            }
            Rule::return_statement => {
                Declaration::ReturnStatement(Self::return_statement(declaration)?)
            }
            Rule::if_statement => Declaration::IfStatement(Self::if_statement(declaration)?),
            Rule::while_loop => Declaration::WhileLoop(Self::while_loop(declaration)?),
            Rule::continue_statement => Declaration::Continue(Self::continue_statement(declaration).to_err_vec()?),
            _ => unreachable!(),
        };

        Ok(matched)

        // Ok(result)
    }
}
