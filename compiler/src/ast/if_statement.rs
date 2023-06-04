use anyhow::{bail, Context, Result};

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{new_err, r#type::IntoType, Block, Compile, CompiledItem, Dependencies, Value};

#[derive(Debug, Clone)]
pub struct IfStatement {
    value: Value,
    body: Block,
    else_statement: Option<ElseStatement>,
}

impl Dependencies for IfStatement {}

impl Compile for IfStatement {
    fn compile(&self, function_buffer: &mut Vec<super::CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut result = vec![];

        let mut value_init = self.value.compile(function_buffer)?;
        result.append(&mut value_init);

        let mut body_init = self.body.compile(function_buffer)?;
        body_init.push(instruction!(done));

        let if_size = body_init.len() + if self.else_statement.is_some() { 2 } else { 1 };

        result.push(instruction!(if if_size));

        if let Some(ref else_statement) = self.else_statement {
            let mut compiled = else_statement.compile(function_buffer)?;

            let len = compiled.len();
            body_init.push(instruction!(jmp len));
            body_init.append(&mut compiled);
        }

        result.append(&mut body_init);

        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub enum ElseStatement {
    Block(Block),
    IfStatement(Box<IfStatement>),
}

impl Compile for ElseStatement {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut content = match self {
            Self::Block(block) => block.compile(function_buffer),
            Self::IfStatement(if_statement) => if_statement.compile(function_buffer),
        }?;

        content.insert(0, instruction!(else));
        content.push(instruction!(done));

        Ok(content)
    }
}

impl Parser {
    pub fn else_statement(input: Node) -> Result<ElseStatement> {
        let child = input.children().next().unwrap();

        let matched = match child.as_rule() {
            Rule::block => ElseStatement::Block(Self::block(child)?),
            Rule::if_statement => ElseStatement::IfStatement(Box::new(Self::if_statement(child)?)),
            _ => unreachable!("{child:?}"),
        };

        Ok(matched)
    }

    pub fn if_statement(input: Node) -> Result<IfStatement> {
        let mut children = input.children();

        let condition = children.next().context("no condition")?;
        let body = children.next().context("no body")?;
        let else_statement = children.next();

        let condition_as_value = Self::value(condition)?;

        let condition_type = condition_as_value.into_type()?;

        if !condition_type.is_boolean() {
            bail!(new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                format!("this value is not boolean, and cannot be used to evaluate an \"if\" statement")
            ))
        }

        input.user_data().push_if();
        let body_as_block = Self::block(body)?;
        input.user_data().pop_scope();

        let else_statement = if let Some(else_statement) = else_statement {
            input.user_data().push_else();
            let x = Some(Self::else_statement(else_statement)?);
            input.user_data().pop_scope();
            x
        } else {
            None
        };

        Ok(IfStatement {
            else_statement,
            value: condition_as_value,
            body: body_as_block,
        })
    }
}
