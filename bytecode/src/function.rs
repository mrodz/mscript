use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::Path;
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::context::Ctx;
use crate::file::MScriptFile;
use crate::instruction::{Instruction, run_instruction};
use crate::arc_to_ref;

use super::instruction::JumpRequest;
use super::stack::{Stack, VariableMapping};
use super::variables::Primitive;

#[derive(Debug, Clone)]
pub struct PrimitiveFunction {
    pub location: String,
    pub callback_state: Option<Arc<VariableMapping>>,
}

impl PrimitiveFunction {
    pub(crate) fn new(path: String, callback_state: Option<Arc<VariableMapping>>) -> Self {
        Self {
            location: path,
            callback_state,
        }
    }

    pub(crate) fn try_new(
        path: String,
        callback_state: Option<Arc<VariableMapping>>,
    ) -> Result<Self> {
        let path_no_function = path.trim_end_matches(|c| c != '#');
        if !Path::exists(&Path::new(&path_no_function[..path_no_function.len() - 1])) {
            bail!("path does not exist ({path_no_function})")
        }

        Ok(Self {
            location: path,
            callback_state,
        })
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
    pub location: Arc<MScriptFile>,
    pub(crate) instructions: Box<[Instruction]>,
    pub name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.get_qualified_name();
        write!(f, "{name} - {} instructions total", self.instructions.len())
    }
}

#[derive(Debug, Clone)]
pub enum ReturnValue {
    FFIError(String),
    NoValue,
    Value(Primitive),
}

impl ReturnValue {
    pub fn get(self) -> Option<Primitive> {
        if let ReturnValue::Value(primitive) = self {
            Some(primitive)
        } else {
            None
        }
    }
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FFIError(message) => write!(f, "{message}"),
            // Self::FFIError { message, length } => unsafe {
            //     let message: &[u8] = slice::from_raw_parts(*message, *length);
            //     let message: Cow<str> = String::from_utf8_lossy(message);

            //     write!(f, "{message}")
            // }
            Self::Value(primitive) => write!(f, "{primitive}"),
            Self::NoValue => write!(f, "None"),
        }
    }
}

// impl Debug for Function {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        
//         // write!(f, "Function {{ name: {:?}, location: {}, line_number: {}, seek_pos: {}, attributes: {:?} }}", self.name, self.location.path, self.line_number, self.seek_pos, self.attributes)
//     }
// }

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // let mut attributes = String::new();

        // for attribute in &self.attributes {
        //     attributes.push_str(&(attribute.to_string() + " "))
        // }

        // let name = self.name.upgrade().expect("the name has been dropped");

        write!(
            f,
            "{} function {}",
            self.location.path,
            self.name,
        )
    }
}

#[derive(Debug, Clone)]
pub enum InstructionExitState {
    ReturnValue(ReturnValue),
    JumpRequest(JumpRequest),
    Goto(usize),
    NoExit,
}

impl<'a> Function {
    pub fn new(
        location: Arc<MScriptFile>,
        // line_number: u32,
        // attributes: Vec<Attributes>,
        name: String,
        // seek_pos: u64,
        instructions: Box<[Instruction]>,
    ) -> Self {
        Self {
            location,
            // line_number,
            // attributes,
            name,
            // seek_pos,
            instructions,
        }
    }

    #[inline]
    pub fn get_qualified_name(&self) -> String {
        format!("{}#{}", self.location.path, self.name)
    }

    pub fn run(
        &mut self,
        args: Vec<Primitive>,
        current_frame: Arc<Stack>,
        callback_state: Option<Arc<VariableMapping>>,
        jump_callback: &mut impl FnMut(JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        arc_to_ref(&current_frame).extend(self.get_qualified_name());
        // unsafe {
        //     (*(Arc::as_ptr(&current_frame) as *mut Stack)).extend(self.get_qualified_name());
        // }

        // let location = self.location;
        // let mut reader = BufReader::new(&*self.location.handle);

        // this takes us to where the function is located in the file.
        // let Ok(pos) = reader.seek(SeekFrom::Start(self.seek_pos)) else {
        // bail!("could not get current file position")
        // };

        let mut context = Ctx::new(&self, current_frame.clone(), args, callback_state);

        // let mut line_number = self.line_number + 2;

        // let mut line = String::new();

        // scope is needed to drop the function context before returning.
        let mut instruction_ptr = 0;

        while instruction_ptr < self.instructions.len() {
            let instruction = &self.instructions[instruction_ptr];
            
            run_instruction(&mut context, &instruction)
                .context("failed to run instruction")
                .with_context(|| format!("{:?}", instruction.name))?;

            let ret = context.poll();

            match ret {
                InstructionExitState::ReturnValue(ret) => {
                    arc_to_ref(&current_frame).pop();
                    return Ok(ret.clone());
                }
                InstructionExitState::JumpRequest(jump_request) => {
                    let result = jump_callback(jump_request).context("Failed to jump")?;

                    if let ReturnValue::FFIError(message) = result {
                        bail!("FFI: {message}")
                    }

                    if let Some(primitive) = result.get() {
                        context.push(primitive)
                    }
                }
                InstructionExitState::Goto(offset) => {
                    instruction_ptr = offset;
                }
                InstructionExitState::NoExit => (),
            }

            context.clear_signal();

            instruction_ptr += 1;
        }

        // Handle when a function does not explicitly return.
        #[cfg(feature = "developer")]
        eprintln!("Warning: function concludes without `ret` instruction");

        arc_to_ref(&current_frame).pop();

        Ok(ReturnValue::NoValue)
    }
}

#[derive(Debug)]
pub struct Functions {
    pub map: HashMap<Arc<String>, Function>,
}

impl<'a> Functions {
    pub fn get_object_functions<'b: 'a>(
        &'a self,
        name: &'b String,
    ) -> impl Iterator<Item = &Function> + 'a {
        self.map.iter().filter_map(move |(key, val)| {
            if key.starts_with(name) {
                Some(val)
            } else {
                None
            }
        })
    }

    pub fn get(&self, signature: &String) -> Result<&Function> {
        let result = self
            .map
            .get(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }

    pub fn get_mut(&'a mut self, signature: &String) -> Result<&'a mut Function> {
        let result = self.map
            .get_mut(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }
}
