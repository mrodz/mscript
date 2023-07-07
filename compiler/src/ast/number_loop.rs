use std::{borrow::Cow, fmt::Display};

use anyhow::{Context, Result};

use crate::{
    ast::{Block, Ident, Value, CompiledItem},
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{Compile, Dependencies, INT_TYPE};

#[derive(Debug)]
pub(crate) struct NumberLoop {
    inclusive: bool,
    val_start: Value,
    val_end: Value,
    step: Option<Value>,
    name: Option<Ident>,
    body: Block,
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

static mut REGISTER_COUNT: usize = 0;

#[derive(Debug)]
enum NumberLoopRegister<'a> {
    Named(&'a str),
    Generated(usize),
}

impl Display for NumberLoopRegister<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.repr())
    }
}

impl<'a> NumberLoopRegister<'a> {
    pub fn new() -> Self {
        unsafe {
            REGISTER_COUNT += 1;
            Self::Generated(REGISTER_COUNT)
        }
    }

    /// Mangles `option` and saves it as a register.
    pub fn from_option(option: Option<&'a Ident>) -> NumberLoopRegister<'a> {
        if let Some(ident) = option {
            Self::Named(ident.name())
        } else {
            Self::new()
        }
    }

    pub fn free(self) {
        unsafe {
            if let Self::Generated(id) = self {
                assert!(REGISTER_COUNT == id);
                REGISTER_COUNT -= 1;
            }
        }
    }

    pub fn repr(&self) -> Cow<str> {
        match self {
            Self::Generated(id) => Cow::Owned(format!("L#{id}")),
            Self::Named(name) => Cow::Borrowed(name),
        }
    }
}

impl Compile for NumberLoop {
    fn compile(
        &self,
        function_buffer: &mut Vec<super::CompiledItem>,
    ) -> Result<Vec<super::CompiledItem>> {
        // let mangled = self.name.as_ref().map(|x| x.mangle());
        let loop_identity = NumberLoopRegister::from_option(self.name.as_ref());

        let mut result = vec![];

        let mut val_start = self.val_start.compile(function_buffer)?;
        let mut val_end = self.val_end.compile(function_buffer)?;

        result.append(&mut val_start);

        result.push(instruction!(store_fast loop_identity));

        result.append(&mut val_end);

        let end_loop_register = NumberLoopRegister::new();

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

        let mut body_compiled = self.body.compile(function_buffer)?;

		let mut step_compiled = vec![instruction!(load loop_identity)];

		if let Some(ref step) = self.step {
			step_compiled.append(&mut step.compile(function_buffer)?)
		} else {
			step_compiled.push(CompiledItem::Instruction {
				id: 0x09,
				arguments: Box::new(["1".to_owned()])
			})
		}

		step_compiled.push(instruction!(bin_op "+"));
		step_compiled.push(instruction!(store loop_identity));

		body_compiled.append(&mut step_compiled);


        let body_len: isize = body_compiled.len().try_into()?;

        const LENGTH_OF_JMP_INSTRUCTION_AND_SPACE: isize = 3;
        let offset_to_end_of_loop: isize = body_len + LENGTH_OF_JMP_INSTRUCTION_AND_SPACE;

        const LENGTH_OF_WHILE_INSTRUCTION: isize = 1;
        const LENGTH_OF_CONDITION: isize = 3;
        let offset_to_start_of_loop: isize =
            -LENGTH_OF_WHILE_INSTRUCTION - LENGTH_OF_CONDITION - body_len;

        result.push(instruction!(while_loop offset_to_end_of_loop));
        result.append(&mut body_compiled);

        result.push(instruction!(jmp_pop offset_to_start_of_loop));

        result.push(instruction!(delete_name_scoped loop_identity end_loop_register));

        end_loop_register.free();
        loop_identity.free();

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

        input.user_data().push_number_loop(child_returns_type);

        let val_start = Self::value(children.next().unwrap())?;
        let inclusive_or_exclusive = children.next().unwrap();
        let val_end = Self::value(children.next().unwrap())?;

        // ^^^ these are guaranteed.

        let mut step: Option<Value> = None;
        let mut name: Option<Ident> = None;
        let mut body: Option<Block> = None;

		for next in children {
            match next.as_rule() {
                Rule::number_loop_step => {
                    step = Some(Self::value(next.children().single().unwrap())?)
                }
                Rule::number_loop_bind_name => {
                    name = {
                        let mut ident = Self::ident(next.children().single().unwrap()).to_err_vec()?;

						ident.link_force_no_inherit(input.user_data(), Cow::Borrowed(&INT_TYPE)).to_err_vec()?;

                        input.user_data().add_dependency(&ident);
                        Some(ident)
                    }
                }
                Rule::block => body = Some(Self::block(next)?),
                rule => unreachable!("{rule:?}"),
            }
        }

        input.user_data().pop_scope();

        let inclusive = inclusive_or_exclusive.as_rule() == Rule::number_loop_inclusive;

        Ok(NumberLoop {
            body: body.context("expected a body").to_err_vec()?,
            name,
            step,
            val_start,
            val_end,
            inclusive,
        })
    }
}
