//! This is the main API for the bytecode interpreter.
//! Use the [Program](self::Program) struct to open a `.mmm` file
//! and spawn the interpreter.

use super::function::ReturnValue;
use super::instruction::{JumpRequest, JumpRequestDestination};
use crate::file::{ExportMap, MScriptFile};
use crate::stack::Stack;
use crate::BytecodePrimitive;
use anyhow::{bail, Context, Result};
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{stdout, Write};
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct Program {
    /// Keeps track of from where the program was called.
    entrypoint: Weak<String>,
    /// Keeps a record of the `.mmm` files in use.
    files_in_use: RefCell<HashMap<Rc<String>, Rc<MScriptFile>>>,
    /// Module cache
    module_cache: RefCell<HashMap<String, RefCell<ExportMap>>>,
}

impl Program {
    fn init_module_cache(
        entrypoint: Weak<String>,
        files_in_use: RefCell<HashMap<Rc<String>, Rc<MScriptFile>>>,
    ) -> Self {
        Self {
            entrypoint,
            files_in_use,
            module_cache: RefCell::default(),
        }
    }

    pub fn new_from_files(
        entrypoint_path: Rc<String>,
        files_in_use: HashMap<Rc<String>, Rc<MScriptFile>>,
    ) -> Result<Self> {
        if !files_in_use.contains_key(&entrypoint_path) {
            bail!(
                "{entrypoint_path} is not the key used for any of these files: {files_in_use:#?}"
            );
        }

        Ok(Self::init_module_cache(
            Rc::downgrade(&entrypoint_path),
            RefCell::new(files_in_use),
        ))
    }

    pub fn new_from_file(entrypoint: Rc<MScriptFile>) -> Self {
        let file_path = entrypoint.path_shared();

        Self::init_module_cache(
            Rc::downgrade(&file_path),
            RefCell::new(HashMap::from([(file_path, entrypoint)])),
        )
    }

    /// Create a new program given a path.
    pub fn new<T>(path: T) -> Result<Self>
    where
        T: Into<String>,
    {
        let path = path.into().replace('\\', "/");
        let entrypoint = Rc::new(path);

        let main_file = MScriptFile::open(Rc::clone(&entrypoint))?;
        let mut files_in_use = HashMap::with_capacity(1);
        files_in_use.insert(Rc::clone(&entrypoint), main_file);

        Ok(Self::init_module_cache(
            Rc::downgrade(&entrypoint),
            RefCell::new(files_in_use),
        ))
    }

    /// Gives callers the ability to check if a file is in use by the interpreter.
    fn is_file_loaded(&self, path: &String) -> Option<Rc<MScriptFile>> {
        self.files_in_use.borrow().get(path).cloned()
    }

    /// Add a file to the running program. Its instructions will be loaded into memory.
    ///
    /// # Returns
    /// * `Ok(false)` - means that the file has already been registered, and no I/O was performed.
    /// * `Ok(true)` - means that a new file was registered.
    ///
    /// # Errors
    /// If opening a `.mmm` file fails, the error will be passed up.
    fn add_file(&self, path: Rc<String>) -> Result<bool> {
        if let Some(file_in_use) = self.files_in_use.borrow().get(&path) {
            let mut view = self.module_cache.borrow_mut();

            use std::borrow::Borrow;
            if !view.contains_key::<String>(path.borrow()) {
                log::info!("Syncing {path}'s exports");
                view.insert(
                    format!("{path}#__module__"),
                    RefCell::new(file_in_use.get_exports()),
                );
            }

            return Ok(false);
        }

        let new_file = MScriptFile::open(Rc::clone(&path))?;

        {
            let mut exports = self.module_cache.borrow_mut();
            exports.insert(
                format!("{path}#__module__"),
                RefCell::new(new_file.get_exports()),
            );

            let mut borrow = self.files_in_use.borrow_mut();
            borrow.insert(path, new_file);
        }

        Ok(true)
    }

    /// Get a reference to a [`MScriptFile`]. The file **must** have already been registered.
    ///
    /// # Errors
    /// Will fail if the file has not already been registered.
    fn get_file(&self, path: &String) -> Result<Rc<MScriptFile>> {
        let Some(file) = self.is_file_loaded(path) else {
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
    fn process_standard_jump_request(&self, request: &JumpRequest) -> Result<ReturnValue> {
        use JumpRequestDestination as JD;
        let (JD::Standard(destination_label) | JD::Module(destination_label)) =
            &request.destination
        else {
            unreachable!()
        };

        let mut last_hash = destination_label.len() - 1;

        for char in destination_label.chars().rev() {
            if char == '#' {
                break;
            }
            last_hash -= 1;
        }

        let (path, label) = destination_label.split_at(last_hash);

        let path = path.to_string().replace('\\', "/");
        let path_ref = &path;

        let added = self.add_file(Rc::new(path.clone()))?;
        log::trace!(
            "encountered file {path_ref}: {}",
            if added { "CREATE" } else { "cached" }
        );
        // rc_to_ref(&rc_of_self).add_file(Rc::new(path.clone()))?;

        let file = self
            .get_file(path_ref)
            .with_context(|| format!("failed jumping to {path}"))?;

        let callback_state: Option<Rc<crate::stack::VariableMapping>> =
            request.callback_state.as_ref().cloned();

        let return_value = file.run_function(
            &label[1..].to_owned(),
            // &symbol.to_owned(),
            Cow::Borrowed(&request.arguments),
            Rc::clone(&request.stack),
            callback_state,
            &mut |req| self.process_jump_request(req),
        )?;
        // let Some(function) = rc_to_ref(&file).get_function(&symbol.to_owned()) else {
        //     bail!("could not find function (missing `{symbol}`, searching in {file:?})")
        // };

        // let return_value = function.run(
        //     Cow::Borrowed(&request.arguments),
        //     Rc::clone(&request.stack),
        //     callback_state,
        //     &mut |req| Self::process_jump_request(rc_of_self.clone(), req),
        // )?;

        // todo!()
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

            let ffi_result = lib_fn(args);

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
    fn process_jump_request(&self, request: &JumpRequest) -> Result<ReturnValue> {
        match &request.destination {
            JumpRequestDestination::Standard(_) => self.process_standard_jump_request(request),
            JumpRequestDestination::Module(path) => {
                {
                    let view = self.module_cache.borrow_mut();

                    if let Some(cached) = view.get(path) {
                        let module = cached.borrow();
                        return Ok(ReturnValue::Value(BytecodePrimitive::Module(
                            module.clone(),
                        )));
                        // log::error!("Probable circular import (caused by {request:?} ... nonrealized cache hit on {view:?})");
                        // bail!("Attempting to import `{path}` gave a partially unitialized module, which is likely a sign of a circular dependency graph")
                    }

                    log::info!("runtime @import cache miss on {path}");
                };

                let result = self.process_standard_jump_request(request)?;

                let ReturnValue::Value(BytecodePrimitive::Module(ref raw_module)) = result else {
                    bail!("{request:?} did not yield a module, but instead {result}");
                };

                let insertion = self
                    .module_cache
                    .borrow_mut()
                    .insert(path.to_owned(), RefCell::new(raw_module.clone()));

                log::trace!(
                    "inserting into module {path}: {}",
                    if insertion.is_some() {
                        "override"
                    } else {
                        "non-override"
                    }
                );

                Ok(result)
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
        let path = self.entrypoint.upgrade().unwrap();

        log::info!("Loading instructions from file...");
        let entrypoint = self.get_file(&path)?;
        log::info!("Loaded instructions from file");

        {
            let mut cache_view = self.module_cache.borrow_mut();
            cache_view.insert(
                format!("{path}#__module__"),
                RefCell::new(entrypoint.get_exports()),
            );
            log::info!("Synced module cache with the entrypoint");
        }

        log::trace!("Creating call stack...");
        let stack = Rc::new(RefCell::new(Stack::new()));
        log::trace!("Created call stack");

        log::debug!("Spawning interpreter...");

        let main_ret = entrypoint
            .run_function(
                &"__module__".to_owned(),
                Cow::Owned(vec![]),
                stack.clone(),
                None,
                &mut |req| self.process_jump_request(req),
            )
            .with_context(|| stack.borrow().to_string());

        if let Err(e) = main_ret {
            stdout().lock().flush()?;
            eprintln!("\n******* MSCRIPT INTERPRETER FATAL RUNTIME ERROR *******\nCall stack trace:\n{e:?}\n\nPlease report this at https://github.com/mrodz/mscript-lang/issues/new\n");
            bail!("Interpreter crashed")
        }

        if stack.borrow().size() != 0 {
            stdout().lock().flush()?;

            eprintln!("\n******* MSCRIPT INTERPRETER STACK MISMATCH *******\nFound:\n{}\n\n... When the program should have unwinded its stack completely. This is most likely a flaw with the compiler.\n\nPlease report this at https://github.com/mrodz/mscript-lang/issues/new\n", stack.borrow());
            bail!("Program exited in a confused state, execution integrity has been compromised.")
        }

        log::info!("Program exited succesfully");

        Ok(())
    }
}
