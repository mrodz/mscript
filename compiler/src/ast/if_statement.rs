use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
    scope::{ScopeHandle, ScopeReturnStatus},
    VecErr,
};

use super::{
    new_err, r#type::IntoType, Block, CompilationState, Compile, CompiledItem, Dependencies, Value,
};

#[derive(Debug)]
pub struct IfStatement {
    value: Value,
    body: Block,
    else_statement: Option<ElseStatement>,
}

impl Dependencies for IfStatement {
    fn dependencies(&self) -> Vec<super::Dependency> {
        let mut value_deps = self.value.net_dependencies();
        value_deps.append(&mut self.body.net_dependencies());

        if let Some(ref else_statement) = self.else_statement {
            value_deps.append(&mut else_statement.net_dependencies());
        }

        value_deps
    }
}

impl Compile for IfStatement {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        let mut result = vec![];

        let mut value_init = self.value.compile(state)?;
        result.append(&mut value_init);

        // result:
        //  - init value

        let mut body_init = self.body.compile(state)?;
        body_init.push(instruction!(done));

        // result:
        //  - value_init...
        // body_init:
        //  - body_init...
        //  - done

        let if_size = body_init.len() + if self.else_statement.is_some() { 3 } else { 1 };

        result.push(instruction!(if if_size));

        // result:
        //  - value_init...
        //  - if ${body_init.len() + else ? 3 : 1}
        // body_init:
        //  - body_init...
        //  - done

        if let Some(ref else_statement) = self.else_statement {
            let mut compiled = else_statement.compile(state)?;

            let len = compiled.len();
            body_init.push(instruction!(jmp len));
            body_init.append(&mut compiled);

            // result:
            //  - value_init...
            //  - if ${body_init.len() + else ? 2 : 1}
            // body_init:
            //  - body_init...
            //  - done
            //  - jmp ${else_len}
            //  - else_statement...
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

impl Dependencies for ElseStatement {
    fn dependencies(&self) -> Vec<super::Dependency> {
        match self {
            Self::Block(block) => block.net_dependencies(),
            Self::IfStatement(if_statement) => if_statement.net_dependencies(),
        }
    }
}

impl Compile for ElseStatement {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        let mut content = match self {
            Self::Block(block) => block.compile(state),
            Self::IfStatement(if_statement) => if_statement.compile(state),
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

        let if_scope: ScopeHandle = input.user_data().push_if_typed(child_returns_type);

        let body_as_block = Self::block(body);

        // get the return type back
        let child_returns_type = if_scope.consume();

        let body_as_block = body_as_block?;

        let do_all_if_branches_return = child_returns_type.all_branches_return();

        let (else_statement, do_all_else_branches_return) =
            if let Some(else_statement) = else_statement {
                let else_scope: ScopeHandle = input.user_data().push_else_typed(child_returns_type);
                let else_statement = Self::else_statement(else_statement);

                // let child_returns_type = input.user_data().pop_scope();
                let child_returns_type = else_scope.consume();

                let x = Some(else_statement?);

                (x, child_returns_type.all_branches_return())
            } else {
                (None, false)
            };

        if do_all_if_branches_return
            && (do_all_else_branches_return || can_determine_is_if_branch_truthy)
        {
            input.user_data().mark_should_return_as_completed();

            // return_type.mark_should_return_as_completed().to_err_vec()?;
        }

        Ok(IfStatement {
            else_statement,
            value: condition_as_value,
            body: body_as_block,
        })
    }
}
