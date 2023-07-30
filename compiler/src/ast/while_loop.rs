use crate::{
    ast::CompiledItem,
    instruction,
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{new_err, r#type::IntoType, Block, Compile, Dependencies, Value};

#[derive(Debug)]
pub struct WhileLoop {
    condition: Value,
    body: Block,
}

impl Compile for WhileLoop {
    fn compile(
        &self,
        function_buffer: &mut Vec<super::CompiledItem>,
    ) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
        let mut condition_compiled = self.condition.compile(function_buffer)?;
        let mut body_compiled = self.body.compile(function_buffer)?;

        const SPACE_FOR_WHILE_AND_JMP_POP: usize = 2;
        condition_compiled.reserve_exact(body_compiled.len() + SPACE_FOR_WHILE_AND_JMP_POP);

        let condition_len: isize = condition_compiled.len().try_into()?;
        let body_len: isize = body_compiled.len().try_into()?;

        const LENGTH_OF_JMP_INSTRUCTION_AND_SPACE: isize = 2;
        let offset_to_end_of_loop: isize = body_len + LENGTH_OF_JMP_INSTRUCTION_AND_SPACE;

        const LENGTH_OF_WHILE_INSTRUCTION: isize = 1;
        let offset_to_start_of_loop: isize =
            -LENGTH_OF_WHILE_INSTRUCTION - body_len - condition_len;

        condition_compiled.push(instruction!(while_loop offset_to_end_of_loop));
        body_compiled.push(instruction!(jmp_pop offset_to_start_of_loop));

        let mut loop_depth = 0;

        let final_body_compiled_len = body_compiled.len();

        for (idx, body_item) in body_compiled.into_iter().enumerate() {
            if body_item.is_loop_instruction() {
                loop_depth += 1;
            }

            if body_item.is_done_instruction() {
                loop_depth -= 1;
            }

            match body_item {
                CompiledItem::Continue(frames_to_pop) if loop_depth == 0 => {
                    let distance_to_end = final_body_compiled_len - idx - 1;

                    let frames_to_pop = frames_to_pop - 1;

                    condition_compiled.push(instruction!(jmp_pop distance_to_end frames_to_pop))
                }
                CompiledItem::Break(frames_to_pop) if loop_depth == 0 => {
                    let distance_to_end = final_body_compiled_len - idx;
                    condition_compiled.push(instruction!(jmp_pop distance_to_end frames_to_pop))
                }
                normal => condition_compiled.push(normal),
            }
            // print!("{}", body_item.repr(true).unwrap());
        }

        // condition_compiled.append(&mut body_compiled);

        Ok(condition_compiled)
    }
}

impl Dependencies for WhileLoop {
    fn dependencies(&self) -> Vec<super::Dependency> {
        let mut condition_dependencies = self.condition.net_dependencies();
        condition_dependencies.append(&mut self.body.net_dependencies());
        condition_dependencies
    }
}

impl Parser {
    pub fn while_loop(input: Node) -> Result<WhileLoop, Vec<anyhow::Error>> {
        let mut children = input.children();

        let condition = children.next().unwrap();
        let condition_span = condition.as_span();
        let block = children.next().unwrap();

        let condition = Self::value(condition)?;

        let condition_ty = condition.for_type().to_err_vec()?;

        if !condition_ty.is_boolean() {
            return Err(vec![new_err(
                condition_span,
                &input.user_data().get_source_file_name(),
                format!("while loop condition must be boolean, found {condition_ty}"),
            )]);
        }

        let child_returns_type = input
            .user_data()
            .return_statement_expected_yield_type()
            .map_or_else(
                || ScopeReturnStatus::No,
                |ty| ScopeReturnStatus::ParentShould(ty.clone()),
            );

        let while_loop_scope = input.user_data().push_while_loop(child_returns_type);

        let body = Self::block(block)?;

        while_loop_scope.consume();

        Ok(WhileLoop { condition, body })
    }
}
