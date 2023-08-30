use std::{borrow::Cow, fmt::Display};

use anyhow::Result;
use pest::Span;

use crate::{
    ast::{Block, CompiledItem, Ident},
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{
    new_err,
    r#type::{IntoType, NativeType},
    BinaryOperation, CompilationState, Compile, Dependencies, TypeLayout, Value, FLOAT_TYPE,
    INT_TYPE,
};

#[derive(Debug)]
pub(crate) struct NumberLoop {
    inclusive: bool,
    val_start: Value,
    val_end: Value,
    step: Option<Value>,
    name: Option<Ident>,
    body: Block,
    name_is_collision: bool,
}

impl Dependencies for NumberLoop {
    fn dependencies(&self) -> Vec<super::Dependency> {
        let mut result = vec![];

        result.append(&mut self.val_start.net_dependencies());
        result.append(&mut self.val_end.net_dependencies());

        if let Some(ref step) = self.step {
            result.append(&mut step.net_dependencies());
        }

        result.append(&mut self.body.net_dependencies());

        result
    }

    fn supplies(&self) -> Vec<super::Dependency> {
        if let Some(ref name) = self.name {
            vec![name.into()]
        } else {
            vec![]
        }
    }
}

#[derive(Debug)]
pub(crate) enum NumberLoopRegister<'a> {
    Named(&'a str),
    Generated(usize),
}

impl<'a> NumberLoopRegister<'a> {
    /// Mangles `option` and saves it as a register.
    pub fn from_option(
        option: Option<&'a Ident>,
        state: &'a CompilationState,
    ) -> NumberLoopRegister<'a> {
        if let Some(ident) = option {
            Self::Named(ident.name())
        } else {
            state.poll_loop_register()
        }
    }

    pub fn free(self, state: &CompilationState) {
        state.free_loop_register(self);
    }
}

impl Display for NumberLoopRegister<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generated(id) => write!(f, "L#{id}"),
            Self::Named(name) => write!(f, "{name}"),
        }
    }
}

impl Compile for NumberLoop {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>> {
        // let mangled = self.name.as_ref().map(|x| x.mangle());
        let loop_identity = NumberLoopRegister::from_option(self.name.as_ref(), state);

        let mut result = vec![];

        let mut val_start = self.val_start.compile(state)?;
        let mut val_end = self.val_end.compile(state)?;

        result.append(&mut val_start);

        result.push(instruction!(store_fast loop_identity));

        result.append(&mut val_end);

        let end_loop_register = state.poll_loop_register();

        result.push(instruction!(store_fast end_loop_register));

        // ^^^ done with bounds init

        result.push(instruction!(load_fast loop_identity));
        result.push(instruction!(load_fast end_loop_register));

        if self.inclusive {
            result.push(instruction!(bin_op "<="))
        } else {
            result.push(instruction!(bin_op "<"))
        }

        // ^^^ condition

        let mut body_compiled = self.body.compile(state)?;

        let mut step_compiled = vec![instruction!(load loop_identity)];

        if let Some(ref step) = self.step {
            step_compiled.append(&mut step.compile(state)?)
        } else {
            step_compiled.push(CompiledItem::Instruction {
                id: 0x09,
                arguments: Box::new(["1".to_owned()]),
            })
        }

        step_compiled.push(instruction!(bin_op "+"));
        step_compiled.push(instruction!(store loop_identity));

        let step_compiled_len = step_compiled.len();

        body_compiled.append(&mut step_compiled);

        let body_len: isize = body_compiled.len().try_into()?;

        const LENGTH_OF_JMP_INSTRUCTION_AND_SPACE: isize = 2;
        let offset_to_end_of_loop: isize = body_len + LENGTH_OF_JMP_INSTRUCTION_AND_SPACE;

        const LENGTH_OF_WHILE_INSTRUCTION: isize = 1;
        const LENGTH_OF_CONDITION: isize = 3;
        let offset_to_start_of_loop: isize =
            -LENGTH_OF_WHILE_INSTRUCTION - LENGTH_OF_CONDITION - body_len;

        result.push(instruction!(while_loop offset_to_end_of_loop));
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
                    let distance_to_end = final_body_compiled_len - step_compiled_len - idx - 1;

                    let frames_to_pop = frames_to_pop - 1;

                    result.push(instruction!(jmp_pop distance_to_end frames_to_pop))
                }
                CompiledItem::Break(frames_to_pop) if loop_depth == 0 => {
                    let distance_to_end = final_body_compiled_len - idx;
                    result.push(instruction!(jmp_pop distance_to_end frames_to_pop))
                }
                normal => result.push(normal),
            }
        }

        if !self.name_is_collision {
            result.push(instruction!(delete_name_scoped loop_identity end_loop_register));
        }

        end_loop_register.free(state);
        loop_identity.free(state);

        Ok(result)
    }
}

impl Parser {
    pub fn number_loop(input: Node) -> Result<NumberLoop, Vec<anyhow::Error>> {
        let mut children = input.children();

        let child_returns_type = input
            .user_data()
            .return_statement_expected_yield_type()
            .map_or_else(
                || ScopeReturnStatus::No,
                |ty| ScopeReturnStatus::ParentShould(ty.clone()),
            );

        let number_loop_scope = input.user_data().push_number_loop(child_returns_type);

        let val_start_node = children.next().unwrap();
        let val_start_span = val_start_node.as_span();
        let val_start = Self::value(val_start_node)?;
        let start_ty = val_start.for_type().to_err_vec()?;

        let inclusive_or_exclusive = children.next().unwrap();

        let val_end_node = children.next().unwrap();
        let val_end_span = val_end_node.as_span();
        let val_end = Self::value(val_end_node)?;
        let end_ty = val_end.for_type().to_err_vec()?;

        // ^^^ these are guaranteed.

        let mut step: Option<(Value, Span)> = None;
        let mut name: Option<Ident> = None;
        let mut body: Option<Block> = None;

        for next in children {
            match next.as_rule() {
                Rule::number_loop_step => {
                    let val = next.children().single().unwrap();
                    let val_span = val.as_span();
                    step = Some((Self::value(val)?, val_span))
                }
                Rule::number_loop_bind_name => {
                    name = {
                        let mut ident =
                            Self::ident(next.children().single().unwrap()).to_err_vec()?;

                        ident
                            .link_force_no_inherit(input.user_data(), Cow::Borrowed(&INT_TYPE))
                            .to_err_vec()?;

                        input.user_data().add_dependency(&ident);
                        Some(ident)
                    }
                }
                Rule::block => body = Some(Self::block(next)?),
                rule => unreachable!("{rule:?}"),
            }
        }

        let rhs = step
            .as_ref()
            .map_or_else(
                || Ok(Cow::Borrowed(*INT_TYPE)),
                |(val, _)| val.for_type().map(Cow::Owned),
            )
            .to_err_vec()?;

        let Some(step_output_type) = start_ty.get_output_type(rhs.as_ref(), &BinaryOperation::Add) else {
            let span = if let Some((_, span)) = step {
                span
            } else {
                val_end_span
            };

            return Err(vec![new_err(
                span,
                &input.user_data().get_source_file_name(),
                format!("attempting to use a step of type `{rhs}`, which cannot be applied to `{start_ty}`"),
            )]);
        };

        let after_step_output: Option<TypeLayout> =
            step_output_type.get_output_type(&start_ty, &BinaryOperation::Lte);

        let Some(TypeLayout::Native(NativeType::Bool)) = after_step_output else {
            return Err(vec![new_err(
                val_start_span,
                &input.user_data().get_source_file_name(),
                format!("applying a step of `{rhs}` to `{start_ty}` produces `{}`", after_step_output.map_or_else(|| Cow::Borrowed("never"), |ty| Cow::Owned(ty.to_string()))),
            )])
        };

        if !start_ty.is_numeric(true) {
            return Err(vec![new_err(
                val_start_span,
                &input.user_data().get_source_file_name(),
                format!("`from` loops only allow numeric bounds, found {start_ty}"),
            )]);
        }

        if !end_ty.is_numeric(true) {
            return Err(vec![new_err(
                val_end_span,
                &input.user_data().get_source_file_name(),
                format!("`from` loops only allow numeric bounds, found {end_ty}"),
            )]);
        }

        let start_is_float = &start_ty == *FLOAT_TYPE;
        let end_is_float = &end_ty == *FLOAT_TYPE;

        if (start_is_float ^ end_is_float) && step.is_none() {
            let span = if start_is_float {
                val_start_span
            } else {
                val_end_span
            };

            return Err(vec![new_err(
                span,
                &input.user_data().get_source_file_name(),
                "using floating point numbers in a `from` loop requires explicitly defining a step property".to_owned(),
            )]);
        }

        number_loop_scope.consume();

        let name_is_collision = name
            .as_ref()
            .map(|ident| input.user_data().has_name_been_mapped(ident.name()))
            .unwrap_or(false);

        let inclusive = inclusive_or_exclusive.as_rule() == Rule::number_loop_inclusive;

        Ok(NumberLoop {
            body: body.unwrap(),
            name,
            step: step.map(|(val, _)| val),
            val_start,
            val_end,
            inclusive,
            name_is_collision,
        })
    }
}
