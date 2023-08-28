//! This is the main API for the bytecode interpreter.
//! Use the [Program](self::Program) struct to open a `.mmm` file
//! and spawn the interpreter.

use super::function::ReturnValue;
use super::instruction::{JumpRequest, JumpRequestDestination};
use super::rc_to_ref;
use crate::file::MScriptFile;
use crate::stack::Stack;
use crate::BytecodePrimitive;
use anyhow::{bail, Context, Result};
use std::borrow::Cow;
use std::collections::HashMap;
use std::panic;
use std::rc::Rc;

/// This struct represents an entire MScript program, from start to finish.
///
/// ```
/// use bytecode::Program;
///
/// let program = Program::new("../examples/bytecode/hello_world/hello_world.mmm").unwrap();
/// program.execute().unwrap();
/// ```
pub struct Program {
    /// Keeps track of from where the program was called.
    entrypoint: Rc<String>,
    /// Keeps a record of the `.mmm` files in use.
    files_in_use: HashMap<String, Rc<MScriptFile>>,
}

impl Program {
    /// Create a new program given a path.
    pub fn new<T>(path: T) -> Result<Self>
    where
        T: Into<String>,
    {
        let path = path.into();
        let main_file = MScriptFile::open(path.clone())?;
        let entrypoint = Rc::new(path.clone());
        let mut files_in_use = HashMap::with_capacity(1);
        files_in_use.insert(path, main_file);

        Ok(Self {
            entrypoint,
            files_in_use,
        })
    }

    /// Gives callers the ability to check if a file is in use by the interpreter.
    fn is_file_loaded(&self, path: &String) -> Option<Rc<MScriptFile>> {
        self.files_in_use.get(path).cloned()
    }

    /// Add a file to the running program. Its instructions will be loaded into memory.
    ///
    /// # Returns
    /// * `Ok(false)` - means that the file has already been registered, and no I/O was performed.
    /// * `Ok(true)` - means that a new file was registered.
    ///
    /// # Errors
    /// If opening a `.mmm` file fails, the error will be passed up.
    fn add_file(&mut self, path: String) -> Result<bool> {
        if self.files_in_use.contains_key(&path) {
            return Ok(false);
        }

        let new_file = MScriptFile::open(path.clone())?;

        self.files_in_use.insert(path, new_file);

        Ok(true)
    }

    /// Get a reference to a [`MScriptFile`]. The file **must** have already been registered.
    ///
    /// # Errors
    /// Will fail if the file has not already been registered.
    fn get_file(self_cell: Rc<Self>, path: &String) -> Result<Rc<MScriptFile>> {
        let Some(file) = self_cell.is_file_loaded(path) else {
            bail!("file is not loaded")
        };

        Ok(file)
    }

    /// API through which the rest of the interpreter's components can request to jump around
    /// to other MScript functions.
    ///
    /// [`Program#process_jump_request`] is a drop-in replacement that is more general, and should
    /// be preferred.
    ///
    /// # Panics
    /// Will panic if `request` is not [`JumpRequestDestination::Standard`], per this function's name.
    fn process_standard_jump_request(
        rc_of_self: Rc<Self>,
        request: &JumpRequest,
    ) -> Result<ReturnValue> {
        let JumpRequestDestination::Standard(ref destination_label) = request.destination else {
            unreachable!()
        };

        let mut last_hash = destination_label.len() - 1;

        for char in destination_label.chars().rev() {
            if char == '#' {
                break;
            }
            last_hash -= 1;
        }

        let (path, symbol) = destination_label.split_at(last_hash);

        let path = path.to_string();
        let symbol = &symbol[1..];

        let path_ref = &path;

        rc_to_ref(&rc_of_self).add_file(path.clone())?;

        let file = Self::get_file(rc_of_self.clone(), path_ref)?;

        let Some(function) = rc_to_ref(&file).get_function(symbol) else {
            bail!("could not find function (missing `{symbol}`)")
        };

        let callback_state = request.callback_state.as_ref().cloned();

        let return_value = function.run(
            Cow::Borrowed(&request.arguments),
            Rc::clone(&request.stack),
            callback_state,
            &mut |req| Self::process_jump_request(rc_of_self.clone(), req),
        )?;

        Ok(return_value)
    }

    /// API through which the rest of the interpreter's components can request to jump around
    /// to dynamically loaded libraries using the [`libloading`] crate under the hood.
    fn process_library_jump_request(
        lib_name: &String,
        func_name: &String,
        args: &[BytecodePrimitive],
    ) -> Result<ReturnValue> {
        use libloading::{Library, Symbol};

        unsafe {
            let lib = Library::new(lib_name)
                .with_context(|| format!("Could not open FFI Library ({lib_name})"))?;
            let lib_fn: Symbol<fn(&[BytecodePrimitive]) -> ReturnValue> = lib
                .get(func_name.as_bytes())
                .with_context(|| format!("Could not find symbol ({func_name})"))?;

            let ffi_result = panic::catch_unwind(|| lib_fn(args));

            let Ok(ffi_result) = ffi_result else {
                bail!("Symbol {func_name} in dynamic library {lib_name} panicked at runtime.\nPlease contact the library owners to remove unwraps, panics, and asserts.\nExceptions and other errors should be sent via a `ReturnValue::FFIError`")
            };

            Ok(ffi_result)
        }
    }

    /// Public-facing API through which interpreter components can jump around to other points of execution.
    /// As of now, the interpreter supports functions and native library calls.
    ///
    /// Calling this function represents a change in control-flow. Thus, the caller accepts
    /// that activities in their scope will freeze until the jump request is completed.
    ///
    /// # Errors
    /// Any errors propagated during the creation/service of the jump request will be bubbled up.
    ///
    /// Any interpreter errors will also be sent upwards and propagated to the caller as well.
    ///
    /// C++ exceptions are undefined behavior, and no support is planned at the moment.
    /// If a user wishes to return the control flow back to the interpreter as an error state,
    /// they should use the [`ReturnValue::FFIError`] variant. Panics in FFI-land will kill the
    /// interpreter by design.
    fn process_jump_request(rc_of_self: Rc<Self>, request: &JumpRequest) -> Result<ReturnValue> {
        match &request.destination {
            JumpRequestDestination::Standard(_) => {
                Self::process_standard_jump_request(rc_of_self, request)
            }
            JumpRequestDestination::Library {
                lib_name,
                func_name,
            } => Self::process_library_jump_request(lib_name, func_name, &request.arguments)
                .context("External error in foreign function interface"),
        }
    }

    /// Start the execution of the program. This function is blocking.
    ///
    /// If this function is `Ok()`, the user's program finished execution without crashing.
    ///
    /// Any errors are propagated upwards.
    pub fn execute(self) -> Result<()> {
        let rc_of_self = Rc::new(self);

        let path = &*rc_of_self.entrypoint;

        log::debug!("Loading instructions from file...");
        let entrypoint = Self::get_file(rc_of_self.clone(), path)?;
        log::debug!("Loaded instructions from file");

        log::debug!("Creating call stack...");
        let stack = Rc::new(Stack::new());
        log::debug!("Created call stack");

        let Some(function) = rc_to_ref(&entrypoint).get_function("main") else {
            bail!("could not find entrypoint (hint: try adding `function main`)")
        };

        log::debug!("Spawning interpreter...");

        let main_ret = function.run(Cow::Owned(vec![]), stack.clone(), None, &mut |req| {
            Self::process_jump_request(rc_of_self.clone(), req)
        });

        if let Err(e) = main_ret {
            eprintln!("\n******* MSCRIPT INTERPRETER FATAL RUNTIME ERROR *******\nCall stack trace:\n{e:?}\n\nPlease report this at https://github.com/mrodz/mscript-lang/issues/new\n");
            bail!("Interpreter crashed")
        }

        if stack.size() != 0 {
            eprintln!("\n******* MSCRIPT INTERPRETER STACK MISMATCH *******\nFound:\n{stack}\n\n... When the program should have unwinded its stack completely. This is most likely a flaw with the compiler.\n\nPlease report this at https://github.com/mrodz/mscript-lang/issues/new\n");
            bail!("Program exited in a confused state, execution integrity has been compromised.")
        }

        log::info!("Program exited succesfully");

        Ok(())
    }
}
