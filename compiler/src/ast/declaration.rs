use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{
    assignment::Assignment, Callable, Compile, CompiledItem, Dependencies, Dependency,
    PrintStatement, r#return::ReturnStatement, if_statement::IfStatement,
};

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    Assignment(Assignment),
    Callable(Callable),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    IfStatement(IfStatement)
}

impl Dependencies for Declaration {
    fn supplies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.supplies(),
            Self::Callable(callable) => callable.supplies(),
            Self::PrintStatement(print_statement) => print_statement.supplies(),
            Self::ReturnStatement(return_statement) => return_statement.supplies(),
            Self::IfStatement(if_statement) => if_statement.supplies(),
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
                // println!("Print Statement");
                print_statement.net_dependencies()
            }
            Self::ReturnStatement(return_statement) => {
                return_statement.net_dependencies()
            }
            Self::IfStatement(if_statement) => {
                if_statement.net_dependencies()
            }
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::PrintStatement(x) => x.compile(function_buffer),
            Self::Callable(x) => x.compile(function_buffer),
            Self::Assignment(x) => x.compile(function_buffer),
            Self::ReturnStatement(x) => x.compile(function_buffer),
            Self::IfStatement(x) => x.compile(function_buffer),
        }
    }
}

impl Parser {
    pub fn declaration(input: Node) -> Result<Declaration> {
        let declaration = input.children().next().unwrap();

        let matched = match declaration.as_rule() {
            Rule::assignment => Declaration::Assignment(Self::assignment(declaration)?),
            Rule::callable => Declaration::Callable(Self::callable(declaration)?),
            Rule::print_statement => {
                Declaration::PrintStatement(Self::print_statement(declaration)?)
            }
            Rule::return_statement => {
                Declaration::ReturnStatement(Self::return_statement(declaration)?)
            }
            Rule::if_statement => {
                Declaration::IfStatement(Self::if_statement(declaration)?)
            }
            _ => unreachable!(),
        };

        Ok(matched)

        // Ok(result)
    }
}
