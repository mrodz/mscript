use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io::{stdout, BufRead, BufReader, Seek, SeekFrom, Write};
use std::ops::{Index, IndexMut};
use std::path::Path;
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::bytecode::context::Ctx;
use crate::bytecode::file::get_line_number_from_pos;
use crate::bytecode::instruction;

use super::attributes_parser::Attributes;
use super::file::IfStatement;
use super::instruction::{run_instruction, Instruction, JumpRequest};
use super::stack::{Stack, VariableMapping};
use super::variables::Primitive;
use super::MScriptFile;

#[derive(Debug, Clone)]
pub struct PrimitiveFunction {
    pub location: String,
    pub callback_state: Option<Arc<VariableMapping>>,
}

impl PrimitiveFunction {
    pub(crate) fn new(path: String, callback_state: Option<Arc<VariableMapping>>) -> Result<Self> {
        let path_no_function = path.trim_end_matches(|c| c != '#');
        if !Path::exists(&Path::new(&path_no_function[..path_no_function.len() - 1])) {
            bail!("path does not exist ({path_no_function})")
        }
        
        Ok(Self { location: path, callback_state })
    }
}

impl PartialOrd for PrimitiveFunction {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        unimplemented!()
    }
}

impl PartialEq for PrimitiveFunction {
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl Display for PrimitiveFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}()", self.location)
    }
}

pub struct Function {
    pub location: Arc<RefCell<MScriptFile>>,
    line_number: u32,
    pub(crate) seek_pos: u64,
    pub(crate) attributes: Vec<Attributes>,
    name: String,
}

#[derive(Debug, Clone)]
pub struct ReturnValue(pub Option<Primitive>);

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

#[derive(Debug, Clone)]
pub enum InstructionExitState {
    ReturnValue(ReturnValue),
    JumpRequest(JumpRequest),
    NewIf(bool),
    GotoElse,
    GotoEndif,
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

    fn run_and_ret<'b>(
        context: &mut Ctx<'a>,
        instruction: &Instruction,
    ) -> Result<InstructionExitState> {
        run_instruction(context, instruction).with_context(|| {
            let _ = stdout().flush();
            format!("failed to run instruction")
        })?;

        // let r#ref = unsafe {
        //     (context.poll() as *const InstructionExitState)
        //         .as_ref()
        //         .unwrap()
        // };
        let r#ref = context.poll();

        Ok(r#ref)
    }

    pub fn get_if_pos_from(&self, if_pos: u64) -> Option<IfStatement> {
        unsafe { (*self.location.as_ptr()).get_if_from(if_pos) }
    }

    pub fn run(
        &mut self,
        args: Vec<Primitive>,
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

        let mut context = Ctx::new(&self, current_frame.clone(), args);

        let mut line_number = self.line_number + 2;

        let mut return_value: ReturnValue = ReturnValue(None);

        let mut line = String::new();

        // scope is needed to drop the function context before returning.
        'function_run: {
            while let Ok(_) = reader.read_line(&mut line) {
                if let Some('\n') = line.chars().next_back() {
                    line.pop();
                }

                if let Some('\r') = line.chars().next_back() {
                    line.pop();
                }

                if line == "end" {
                    break;
                }

                let instruction = instruction::parse_line(&line).context("failed parsing line")?;

                let ret = Self::run_and_ret(&mut context, &instruction)
                    .with_context(|| format!("`{}` on line {line_number}", instruction.name))?;

                match ret {
                    InstructionExitState::ReturnValue(ret) => {
                        return_value = ret.clone();
                        break 'function_run;
                    }
                    InstructionExitState::JumpRequest(jump_request) => {
                        let result = jump_callback(jump_request)?;

                        if let Some(primitive) = result.0 {
                            context.push(primitive)
                        }
                    }
                    InstructionExitState::NewIf(used) => {
                        let pos = reader.stream_position()?;
                        let if_stmt = self
                            .get_if_pos_from(pos)
                            .expect("this if has not been mapped.");

                        let IfStatement::If(..) = if_stmt else {
                            bail!("expected if statment, found {if_stmt:?}");
                        };

                        let next = *if_stmt.next_pos();

                        context.active_if_stmts.push((if_stmt, used));

                        if !used {
                            reader.seek(SeekFrom::Start(next))?;
                        }
                    }
                    InstructionExitState::GotoElse => {
                        let (if_stmt, used) =
                            context.active_if_stmts.last().with_context(|| {
                                format!(
                                    "no if, but found else (l{})",
                                    get_line_number_from_pos(&mut reader, pos).unwrap()
                                )
                            })?;

                        if *used {
                            let IfStatement::If(_, box IfStatement::Else(_, box IfStatement::EndIf(next_pos))) = if_stmt else {
                                bail!("bad ({if_stmt:?})");
                            };

                            reader.seek(SeekFrom::Start(*next_pos))?;
                        }

                        context.active_if_stmts.pop();
                        context.clear_stack();
                    }
                    InstructionExitState::GotoEndif => {
                        context.active_if_stmts.pop();
                        context.clear_stack();
                    }
                    InstructionExitState::NoExit => (),
                }

                context.clear_signal();

                line_number += 1;

                line.clear();
            }
        }

        unsafe {
            (*current_frame.as_ptr()).pop();
        }

        Ok(return_value)
    }
}

pub struct Functions {
    pub map: HashMap<String, Function>,
    pub if_mapper: HashMap<u64, IfStatement>,
}

impl<'a> Functions {
    pub fn get(&self, signature: &str) -> Result<&Function> {
        let Some(result) = self.map.get(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }

    pub fn get_mut(&'a mut self, signature: &str) -> Result<&'a mut Function> {
        let Some(result) = self.map.get_mut(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }
}

impl<'a> Index<&'a str> for Functions {
    type Output = Function;
    fn index(&self, index: &'a str) -> &Self::Output {
        self.map.get(index).unwrap()
    }
}

impl<'a> IndexMut<&'a str> for Functions {
    fn index_mut(&mut self, index: &'a str) -> &mut Self::Output {
        self.map.get_mut(index).unwrap()
    }
}
