//! Everything to do with functions is found here:
//! * Execution of loaded bytecode functions ([`Function`])
//! * Representation of runtime callbacks/closures ([`PrimitiveFunction`])

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::Path;
use std::rc::Rc;

use anyhow::{bail, Context, Result};

use crate::compilation_lookups::raw_byte_instruction_to_string_representation;
use crate::context::{Ctx, SpecialScope};
use crate::file::MScriptFile;
use crate::instruction::{run_instruction, Instruction};
use crate::rc_to_ref;

use super::instruction::JumpRequest;
use super::stack::{Stack, VariableMapping};
use super::variables::Primitive;

/// A callback/closure whose variables are mapped at runtime.
/// This struct has no direct _functionality_; it is only meant
/// to store relevant data that can be operated upon.
///
/// While it may seem intuitive, it should be noted that there is
/// no way to call a callback from an FFI interface. Once the interpreter
/// moves to FFI-land, all MScript code will wait until the blocking
/// library finishes its execution.
#[derive(Debug, Clone)]
pub struct PrimitiveFunction {
    /// Used to keep track of where to jump when this function gets called.
    location: String,
    /// Each callback has an `Rc` to a [`VariableMapping`]. In other words, a shared
    /// reference to a map of shared references to variables. This allows multiple instances
    /// of the same callback to operate on the same data in a thread-safe manner,
    /// but there is a notable hit to performance.
    callback_state: Option<Rc<VariableMapping>>,
}

impl PrimitiveFunction {
    /// Initialize a [`PrimitiveFunction`] given its fields.
    pub(crate) fn new(path: String, callback_state: Option<Rc<VariableMapping>>) -> Self {
        Self {
            location: path,
            callback_state,
        }
    }

    /// Initialize a [`PrimitiveFunction`] given its fields. This function will
    /// validate that the path to the function exists before its creation.
    ///
    /// # Errors
    /// This function will propagate I/O errors from traversing symlinks in seRch of
    /// the file. It will also error if the file does not exist.
    pub(crate) fn try_new(
        path: String,
        callback_state: Option<Rc<VariableMapping>>,
    ) -> Result<Self> {
        // get the file system path from an MScript function path.
        // ie. path/to/file.mmm#__fn0
        //     ^^^^^^^^^^^^^^^^
        let path_no_function = path.trim_end_matches(|c| c != '#');

        if !Path::try_exists(Path::new(&path_no_function[..path_no_function.len() - 1]))? {
            bail!("path does not exist ({path_no_function})")
        }

        Ok(Self {
            location: path,
            callback_state,
        })
    }

    /// Get the location of the function.
    pub(crate) fn location(&self) -> &String {
        &self.location
    }

    /// Get the variables mapped to this closure.
    pub(crate) fn callback_state(&self) -> &Option<Rc<VariableMapping>> {
        &self.callback_state
    }
}

impl PartialEq for PrimitiveFunction {
    /// Avoid comparing the variable mapping, which should always be the same
    /// in the case where [`PrimitiveFunction::location`]'s are equal.
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl Display for PrimitiveFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function ptr {}()", self.location)?;

        if let Some(ref callback_state) = self.callback_state {
            write!(f, " + pool@{:#x}", Rc::as_ptr(callback_state) as usize)?;
        };

        Ok(())
    }
}

/// This enum represents the return state of a function.
/// As of now, the only valid states are:
/// * `FFIError` (produced **exclusively** by the `raise_error!` macro)
/// * `NoValue`
/// * `Value`
#[derive(Debug, Clone)]
pub enum ReturnValue {
    /// This variant is produced when calling an FFI function produces an expected error,
    /// as opposed to directly panicking. While rust FFI `panic!`'s should be caught by a
    /// [`std::panic::catch_unwind`], other exceptions and branches in control flow
    /// (like C++ exceptions, `longjmp`, `setjmp`) will produce undefined behavior.
    FFIError(String),
    /// Produced by functions that return `()`/`void`
    NoValue,
    /// This variant is produced when a function exits succesfully and produces a value.
    Value(Primitive),
}

impl ReturnValue {
    /// Get the return value of this function, but only if one exists.
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
            Self::Value(primitive) => write!(f, "{primitive}"),
            Self::NoValue => write!(f, "None"),
        }
    }
}

/// This enum represents the valid return states of a **single** bytecode instruction.
///
/// All [`InstructionExitState`] variants can be sent up from an [`Instruction`] to its
/// parent [`Function`] via the [`Ctx`] bridge.
///
/// When doing so, the flag sent can be viewed as a request for the interpreter to:
/// * execute code
/// * change its behavior
/// * jump around
///
/// A key word is **request**. The interpreter does not have to heed every request,
/// but most requests are infallible and guaranteed to be processed.
#[derive(Debug)]
pub enum InstructionExitState {
    /// This variant serves to signal that an instruction has a [`ReturnValue`] it would
    /// like the function to deal with.
    ReturnValue(ReturnValue),
    /// This variant serves to signal that an instruction has a [`JumpRequest`] it would
    /// like the function to deal with.
    JumpRequest(JumpRequest),
    /// This variant signals that an instruction would like the interpreter to jump forward
    /// `+isize` instructions.
    ///
    /// # Errors
    /// This request will fail if the requested interpreter cursor
    /// position would fall out of bounds.
    Goto(isize),
    /// This variant requests to push a [`SpecialScope`] to the interpreter process. This scope
    /// can be used for identification.
    PushScope(SpecialScope),
    /// This variant requests to:
    /// * push a [`SpecialScope`] to the interpreter process
    /// * jump forward `+usize` instructions.
    ///
    /// # Errors
    /// This request will fail if the requested interpreter cursor
    /// position would fall out of bounds.
    GotoPushScope(usize, SpecialScope),
    /// This variant requests to:
    /// * pop a [`SpecialScope`] from the interpreter process
    /// * jump forward `+usize` instructions.
    GotoPopScope(isize, usize),
    /// This variant requests to pop a [`SpecialScope`] from the interpreter process.
    ///
    /// # Errors
    /// This request will fail if the interpreter does not have any special scopes on its stack.
    PopScope,
    /// # Default
    ///
    /// This is the standard exit state. The interpreter will move on to the next instruction
    /// if it encounters this variant.
    NoExit,
}

/// This is MScript's main unit of executing instructions.
///
/// Along with a function's name and location, this struct
/// packs together all of the bytecode instructions needed to run
/// the code.
///
/// In essense, this struct exposes an interface that allows users to
/// run raw bytecode. (See `Function::run()`)
pub struct Function {
    /// A shared reference to the file of origin.
    location: Rc<MScriptFile>,
    /// A list of the instructions this subroutine consists of.
    instructions: Box<[Instruction]>,
    /// The name of this function.
    name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.get_qualified_name();
        write!(f, "{name} - {} instructions total", self.instructions.len())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} function {}", self.location.path(), self.name,)
    }
}

impl Function {
    /// Initialize a [`Function`] given its fields.
    pub(crate) fn new(
        location: Rc<MScriptFile>,
        name: String,
        instructions: Box<[Instruction]>,
    ) -> Self {
        Self {
            location,
            name,
            instructions,
        }
    }

    /// Get the "representation" of a [`Function`]. In other words,
    /// combine a [`Function`]'s path with its name to create an MScript Function Path.
    ///
    /// # Examples
    /// ```ignore
    /// use std::sync::Rc;
    ///
    /// let file = MScriptFile::open("path/to/file.mmm")?;
    /// let instructions = ...;
    /// let function = Function::new(Rc::new(file), "add_numbers".into(), instructions);
    ///
    /// assert_eq!(function.get_qualified_name(), "path/to/file.mmm#add_numbers")
    /// ```
    #[inline]
    pub(crate) fn get_qualified_name(&self) -> String {
        format!("{}#{}", self.location.path(), self.name)
    }

    /// This is the backbone of the interpreter. This function creates the main event
    /// loop for each function, and handles jumps. Because of this, it is **very complex**
    /// and has many ways to diverge.
    ///
    /// # Arguments
    /// * `args` - The arguments to this function, provided by the caller.
    /// * `current_frame` - A shared reference to the current stack trace. The caller
    ///                     **SHOULD NOT** push a stack frame for a bytecode function
    ///                     before calling it; this method will handle that.
    /// * `callback_state` - A shared reference to the [`VariableMapping`] that this
    ///                      function can access. This argument is used for callbacks
    ///                      and closures exclusively. Normal variables should be
    ///                      added to `current_frame`.
    /// * `jump_callback` - A callback that defines how this function's jumps are handled.
    ///                     The implementation is up to the caller.
    ///
    /// # Errors
    /// This function can error if an instruction raises an error during execution.
    ///
    /// # Panics
    /// This function will `panic!` if the instruction byte falls outside of (0..[`INSTRUCTION_COUNT`][crate::instruction_constants::INSTRUCTION_COUNT])
    pub(crate) fn run(
        &mut self,
        args: Cow<Vec<Primitive>>,
        current_frame: Rc<Stack>,
        callback_state: Option<Rc<VariableMapping>>,
        jump_callback: &mut impl Fn(&JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        rc_to_ref(&current_frame).extend(self.get_qualified_name());

        // Each function needs its own context.
        let mut context = Ctx::new(self, current_frame.clone(), args, callback_state);

        // The index into the instruction array.
        let mut instruction_ptr = 0;

        let mut special_scopes: Vec<SpecialScope> = vec![];

        while instruction_ptr < self.instructions.len() {
            let instruction = &self.instructions[instruction_ptr];

            // queries the function pointer associated with the instruction,
            // and gives it ownership of the instruction.
            run_instruction(&mut context, instruction)
                .context("failed to run instruction")
                .with_context(|| {
                    let instruction_as_str =
                        raw_byte_instruction_to_string_representation(instruction.id)
                            .unwrap_or(Cow::Borrowed("Unknown Instruction"));

                    format!(
                        "{instruction_as_str:?} (instruction #{instruction_ptr} of {})",
                        self.name
                    )
                })?;

            // `context` must have its exit state cleared before continuing the loop.
            let ret: &InstructionExitState = context.poll();

            let mut goto_fn = |offset: isize| -> Result<()> {
                let new_val = instruction_ptr.checked_add_signed(offset).with_context(|| format!("numeric overflow ({instruction_ptr} + {offset})"))?;

                let instruction_len = self.instructions.len();

                if new_val >= instruction_len {
                    bail!("goto position index {new_val} is too big, instruction length is {instruction_len}.");
                }

                // println!("\twent from {instruction_ptr} to {new_val}");

                instruction_ptr = new_val;
                Ok(())
            };

            // process the exit state
            match ret {
                InstructionExitState::ReturnValue(ret) => {
                    rc_to_ref(&current_frame).pop_until_function();
                    return Ok(ret.clone());
                }
                InstructionExitState::JumpRequest(jump_request) => {
                    let result = jump_callback(jump_request)
                        .with_context(|| context.get_call_stack_string())?;

                    if let ReturnValue::FFIError(message) = result {
                        bail!("FFI: {message}")
                    }

                    if let Some(primitive) = result.get() {
                        context.push(primitive)
                    }
                }
                InstructionExitState::Goto(offset) => {
                    goto_fn(*offset)?;
                    context.clear_signal();
                    continue;
                }
                InstructionExitState::PushScope(ty) => {
                    special_scopes.push(*ty);
                    context.add_frame(ty.to_string());
                }
                InstructionExitState::GotoPushScope(offset, ty) => {
                    goto_fn((*offset).try_into()?)?;
                    
                    special_scopes.push(*ty);
                    context.add_frame(ty.to_string());

                    context.clear_signal();
                    continue;
                }
                InstructionExitState::GotoPopScope(offset, frames_to_pop) => {
                    goto_fn(*offset)?;

                    for _ in 0..*frames_to_pop {
                        context.pop_frame();
                    }

                    context.clear_signal();
                    continue;                    

                }
                InstructionExitState::PopScope => {
                    if special_scopes.pop().is_some() {
                        context.pop_frame();
                    }
                }
                InstructionExitState::NoExit => (),
            }

            context.clear_signal();

            instruction_ptr += 1;
        }

        // Handle when a function does not explicitly return.
        #[cfg(feature = "developer")]
        eprintln!("Warning: function concludes without `ret` instruction");

        rc_to_ref(&current_frame).pop();

        Ok(ReturnValue::NoValue)
    }

    /// Get a function's name.
    pub(crate) fn name(&self) -> &String {
        &self.name
    }

    /// Get a function's location.
    pub(crate) fn location(&self) -> &Rc<MScriptFile> {
        &self.location
    }
}

/// A map of a function's name to the function struct.
#[derive(Debug)]
pub(crate) struct Functions {
    map: HashMap<Rc<String>, Function>,
}

impl<'a> Functions {
    /// Initialize a [`Functions`] map given its fields.
    pub(crate) fn new(map: HashMap<Rc<String>, Function>) -> Self {
        Self { map }
    }

    /// Get all functions whose name starts with `name`
    pub(crate) fn get_object_functions(
        &'a self,
        name: &'a String,
    ) -> impl Iterator<Item = &Function> + 'a {
        self.map.iter().filter_map(move |(key, val)| {
            if key.starts_with(name) {
                Some(val)
            } else {
                None
            }
        })
    }

    /// Get a reference to a function with name `signature`, if it exists.
    pub(crate) fn get(&self, signature: &String) -> Result<&Function> {
        let result = self
            .map
            .get(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }

    /// Get a mutable reference to a function with name `signature`, if it exists.
    pub(crate) fn get_mut(&'a mut self, signature: &String) -> Result<&'a mut Function> {
        let result = self
            .map
            .get_mut(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }
}
