use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::{assignment::Assignment, Callable, Compile, Dependencies, Dependency, PrintStatement, CompiledItem};

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    Assignment(Assignment),
    Callable(Callable),
    PrintStatement(PrintStatement),
}

impl Dependencies for Declaration {
    fn supplies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.supplies(),
            Self::Callable(callable) => callable.supplies(),
            Self::PrintStatement(print_statement) => print_statement.supplies(),
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
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        match self {
            Self::PrintStatement(x) => x.compile(function_buffer),
            Self::Callable(x) => x.compile(function_buffer),
            Self::Assignment(x) => x.compile(function_buffer),
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
            _ => unreachable!(),
        };

        Ok(matched)

        // Ok(result)
    }
}
