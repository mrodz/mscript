use std::fmt::Debug;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
    VecErr,
};

use super::{
    r#type::TypeAlias, Assertion, Assignment, Break, Class, CompilationState, Compile,
    CompiledItem, Continue, Dependencies, Dependency, IfStatement, Import, NumberLoop,
    PrintStatement, Reassignment, ReturnStatement, Value, WhileLoop,
};

#[derive(Debug)]
pub(crate) enum Declaration {
    Assignment(Assignment),
    Reassignment(Reassignment),
    // Expr(Expr),
    PrintStatement(PrintStatement),
    ReturnStatement(ReturnStatement),
    IfStatement(IfStatement),
    WhileLoop(WhileLoop),
    NumberLoop(NumberLoop),
    Continue(Continue),
    Break(Break),
    Assertion(Assertion),
    Class(Class),
    Value(Value),
    Import(Import),
    TypeAlias(TypeAlias),
}

impl Dependencies for Declaration {
    fn supplies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.supplies(),
            Self::PrintStatement(print_statement) => print_statement.supplies(),
            Self::ReturnStatement(return_statement) => return_statement.supplies(),
            Self::IfStatement(if_statement) => if_statement.supplies(),
            Self::WhileLoop(while_loop) => while_loop.supplies(),
            Self::NumberLoop(number_loop) => number_loop.supplies(),
            Self::Assertion(assertion) => assertion.supplies(),
            Self::Class(class) => class.supplies(),
            Self::Value(value) => value.supplies(),
            Self::Reassignment(reassignment) => reassignment.supplies(),
            Self::Import(import) => import.supplies(),
            Self::TypeAlias(type_alias) => type_alias.supplies(),
            Self::Continue(_) | Self::Break(_) => vec![],
        }
    }

    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            Self::Assignment(assignment) => assignment.net_dependencies(),
            Self::PrintStatement(print_statement) => print_statement.net_dependencies(),
            Self::ReturnStatement(return_statement) => return_statement.net_dependencies(),
            Self::IfStatement(if_statement) => if_statement.net_dependencies(),
            Self::WhileLoop(while_loop) => while_loop.net_dependencies(),
            Self::NumberLoop(number_loop) => number_loop.net_dependencies(),
            Self::Reassignment(reassignment) => reassignment.net_dependencies(),
            Self::Assertion(assertion) => assertion.net_dependencies(),
            Self::Class(class) => class.net_dependencies(),
            Self::Value(value) => value.net_dependencies(),
            Self::Import(import) => import.net_dependencies(),
            Self::TypeAlias(type_alias) => type_alias.net_dependencies(),
            Self::Continue(_) | Self::Break(_) => vec![],
        }
    }
}

impl Compile for Declaration {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        match self {
            Self::PrintStatement(x) => x.compile(state),
            Self::Assignment(x) => x.compile(state),
            Self::Reassignment(x) => x.compile(state),
            Self::ReturnStatement(x) => x.compile(state),
            Self::IfStatement(x) => x.compile(state),
            Self::WhileLoop(x) => x.compile(state),
            Self::Continue(x) => x.compile(state),
            Self::Break(x) => x.compile(state),
            Self::NumberLoop(x) => x.compile(state),
            Self::Assertion(x) => x.compile(state),
            Self::Class(x) => x.compile(state),
            Self::Value(x) => {
                let mut result = x.compile(state)?;
                result.push(instruction!(void));
                Ok(result)
            }
            Self::Import(x) => x.compile(state),
            Self::TypeAlias(x) => x.compile(state),
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
            Rule::reassignment => Declaration::Reassignment(Self::reassignment(declaration)?),
            Rule::assertion => Declaration::Assertion(Self::assertion(declaration)?),
            Rule::class => Declaration::Class(Self::class(declaration)?),
            Rule::value => Declaration::Value(Self::value(declaration)?),
            Rule::import => Declaration::Import(Self::import(declaration)?),
            Rule::type_alias => Declaration::TypeAlias(Self::type_alias(declaration).to_err_vec()?),
            x => unreachable!("{x:?} is not supported"),
        };

        Ok(matched)
    }
}
