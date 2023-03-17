use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io::{stdout, BufRead, BufReader, Seek, SeekFrom, Write};
use std::ops::{Index, IndexMut};
use std::{fs::File, sync::Arc};

use anyhow::{bail, Context, Result};

use crate::bytecode::instruction;

use super::attributes_parser::Attributes;
use super::instruction::{run_instruction, Ctx, Instruction, JumpRequest};
use super::stack::Stack;
use super::variable::Primitive;
use super::MScriptFile;

pub struct Function {
    location: Arc<RefCell<MScriptFile>>,
    // location: Arc<String>,
    // location_handle: Arc<File>,
    line_number: u32,
    pub(crate) seek_pos: u64,
    attributes: Vec<Attributes>,
    name: String,
}

#[derive(Debug)]
pub struct ReturnValue(Option<Primitive>);

impl ReturnValue {
    pub fn get(&self) -> &Option<Primitive> {
        &self.0
    }
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(var) = self.get() {
            write!(f, "{var}")
        } else {
            write!(f, "None")
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function {{ name: {:?}, location: {}, line_number: {}, seek_pos: {}, attributes: {:?} }}", self.name, self.location.borrow().path, self.line_number, self.seek_pos, self.attributes)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut attributes = String::new();

        for attribute in &self.attributes {
            attributes.push_str(&(attribute.to_string() + " "))
        }

        write!(
            f,
            "{}@@{}:{}@@ function {} ({:#x})",
            attributes,
            self.location.borrow().path,
            self.line_number + 1,
            self.name,
            self.seek_pos
        )
    }
}

pub enum InstructionExitState {
    ReturnValue(ReturnValue),
    JumpRequest(JumpRequest),
    NoExit,
}

impl<'a> Function {
    pub fn new(
        location: Arc<RefCell<MScriptFile>>,
        line_number: u32,
        attributes: Vec<Attributes>,
        name: String,
        seek_pos: u64,
    ) -> Self {
        Self {
            location,
            line_number,
            attributes,
            name,
            seek_pos,
        }
    }

    #[inline]
    pub fn get_qualified_name(&self) -> String {
        format!("{}#{}", self.location.borrow().path, self.name)
    }

    fn run_and_ret(
        context: &mut Ctx<'a>,
        instruction: &Instruction,
    ) -> Result<InstructionExitState> {
        run_instruction(context, instruction).with_context(|| {
            let _ = stdout().flush();
            format!("failed to run instruction")
        })?;

        if let Ok(ret) = context.get_return_value() {
            return Ok(InstructionExitState::ReturnValue(ReturnValue(ret.clone())));
        } else {
            if let Some(jump_request) = context.clear_and_get_jump_request() {
                return Ok(InstructionExitState::JumpRequest(jump_request));
            }
        }

        Ok(InstructionExitState::NoExit)
    }

    pub fn run(
        &mut self,
        current_frame: Arc<Cell<Stack>>,
        jump_callback: &mut impl FnMut(JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        {
            unsafe {
                (*current_frame.as_ptr()).extend(self.get_qualified_name());
            }
        }

        let location = self.location.borrow();
        let mut reader = BufReader::new(location.handle.as_ref());

        let Ok(pos) = reader.seek(SeekFrom::Start(self.seek_pos)) else {
            bail!("could not get current file position")
        };

        assert_eq!(pos, self.seek_pos);

        let mut context = Ctx::new(&self, current_frame.clone());

        let mut line_number = self.line_number + 2;

        let lines = reader.lines();

        let mut return_value: ReturnValue = ReturnValue(None);

        // scope is needed to drop the function context before returning.
        'function_run: {
            for line in lines {
                if line.is_err() {
                    bail!("could not read function:\n\t{}", self)
                }

                let line = line.unwrap();

                if line == "end" {
                    break;
                }

                let instruction = instruction::parse_line(&line).context("failed parsing line")?;

                let ret = Self::run_and_ret(&mut context, &instruction)
                    .with_context(|| format!("`{}` on line {line_number}", instruction.name))?;

                match ret {
                    InstructionExitState::ReturnValue(ret) => {
                        return_value = ret;
                        break 'function_run;
                    }
                    InstructionExitState::JumpRequest(jump_request) => {
                        let result = jump_callback(jump_request)?;

                        if let Some(primitive) = result.0 {
                            context.push(primitive)
                        }
                    }
                    InstructionExitState::NoExit => (),
                    _ => unimplemented!(),
                }

                line_number += 1;
            }
        }

        unsafe {
            (*current_frame.as_ptr()).pop();
        }
        
        Ok(return_value)
    }
}

pub struct Functions(pub HashMap<String, Function>);

impl<'a> Functions {
    pub fn get(&self, signature: &str) -> Result<&Function> {
        let Some(result) = self.0.get(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }

    pub fn get_mut(&'a mut self, signature: &str) -> Result<&'a mut Function> {
        let Some(result) = self.0.get_mut(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }
}

impl<'a> Index<&'a str> for Functions {
    type Output = Function;
    fn index(&self, index: &'a str) -> &Self::Output {
        self.0.get(index).unwrap()
    }
}

impl<'a> IndexMut<&'a str> for Functions {
    fn index_mut(&mut self, index: &'a str) -> &mut Self::Output {
        self.0.get_mut(index).unwrap()
    }
}
