use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{new_err, r#type::IntoType, Block, Compile, CompiledItem, Dependencies, Value};

#[derive(Debug)]
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

#[derive(Debug)]
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
    pub fn else_statement(input: Node) -> Result<ElseStatement, Vec<anyhow::Error>> {
        let child = input.children().next().unwrap();

        let matched = match child.as_rule() {
            Rule::block => ElseStatement::Block(Self::block(child)?),
            Rule::if_statement => ElseStatement::IfStatement(Box::new(Self::if_statement(child)?)),
            _ => unreachable!("{child:?}"),
        };

        Ok(matched)
    }

    pub fn if_statement(input: Node) -> Result<IfStatement, Vec<anyhow::Error>> {
        let mut children = input.children();

        let condition = children.next().expect("no condition");
        let body = children.next().expect("no body");
        let else_statement = children.next();

        let condition_as_value = Self::value(condition)?;

        let condition_type = condition_as_value.for_type().to_err_vec()?;

        if !condition_type.is_boolean() {
            return Err(vec![new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "this value is not boolean, and cannot be used to evaluate an \"if\" statement"
                    .to_owned(),
            )]);
        }

        let can_determine_is_if_branch_truthy = false; // TODO

        let child_returns_type = input
            .user_data()
            .return_statement_expected_yield_type()
            .map_or_else(
                || ScopeReturnStatus::No,
                |ty| ScopeReturnStatus::ParentShould(ty.clone()),
            );

        input.user_data().push_if_typed(child_returns_type);
        let body_as_block = Self::block(body)?;
        // get the return type back
        let child_returns_type = input.user_data().pop_scope();

        let do_all_if_branches_return = child_returns_type.all_branches_return();

        let (else_statement, do_all_else_branches_return) =
            if let Some(else_statement) = else_statement {
                input.user_data().push_else_typed(child_returns_type);
                let x = Some(Self::else_statement(else_statement)?);
                let child_returns_type = input.user_data().pop_scope();

                (x, child_returns_type.all_branches_return())
            } else {
                (None, false)
            };

        if do_all_if_branches_return
            && (do_all_else_branches_return || can_determine_is_if_branch_truthy)
        {
            input
                .user_data()
                .get_return_type()
                .mark_should_return_as_completed()
                .to_err_vec()?;
        }

        Ok(IfStatement {
            else_statement,
            value: condition_as_value,
            body: body_as_block,
        })
    }
}
