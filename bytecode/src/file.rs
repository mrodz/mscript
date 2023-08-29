//! Interface for file operations.

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::rc::Rc;

use anyhow::{bail, Context, Result};

use crate::function::Function;
use crate::instruction::{split_string, Instruction};

use super::function::Functions;
use super::rc_to_ref;

/// Wrapper around a bytecode file.
#[derive(Debug)]
pub struct MScriptFile {
    /// A shared reference to the path to the file, used for debug and identification purposes.
    path: Rc<String>,
    /// The functions in the file. Even though it is an `Option`, by the time an [`MScriptFile`]
    /// is initialized, this field will be propagated.
    functions: Option<Functions>,
}

#[derive(Debug)]
pub struct MScriptFileBuilder {
    building: Rc<MScriptFile>,
}

impl MScriptFileBuilder {
    pub fn new(path_to_file: String) -> Self {
        Self {
            building: Rc::new(MScriptFile {
                path: Rc::new(path_to_file),
                functions: Some(Functions::new_empty()),
            }),
        }
    }

    pub fn add_function(&mut self, name: String, bytecode: Box<[Instruction]>) -> Option<Function> {
        let Some(ref mut functions) = rc_to_ref(&self.building).functions else {
            unreachable!()
        };

        functions.add_function(Rc::downgrade(&self.building), name, bytecode)
    }

    pub fn build(self) -> Rc<MScriptFile> {
        self.building
    }
}

/// Return how many lines (identified by ASCII newlines) are found between a
/// file seek position and SOI.
///
/// This is an expensive call. Should only be used for errors or non-build debugging.
///
/// # Arguments
///
/// * `reader` - A mutable reference to a [`BufReader<&File>`]
/// * `pos` - The current cursor position of the `reader`
///
/// # Errors
///
/// If this function encounters any form of I/O error, an error
/// variant will be returned. If an error is returned then there is a chance
/// the file's seek position is not where it was prior to the function call,
/// and should be checked.
///
/// # Todo
/// There is probably a more performant way to get the line number. For now,
/// this works.
pub fn get_line_number_from_pos(reader: &mut BufReader<&File>, pos: u64) -> Result<u64> {
    let old_pos = reader.stream_position()?;
    reader.seek(SeekFrom::Start(0))?;

    let mut result = 0;

    let mut buffer = String::new();

    loop {
        reader.read_line(&mut buffer)?;

        result += 1;

        // we've passed pos, aka. it is in this line.
        if reader.stream_position()? >= pos {
            break;
        }
    }

    // reset after completion
    reader.seek(SeekFrom::Start(old_pos))?;

    Ok(result)
}

impl MScriptFile {
    /// Open a bytecode file given its path. This method opens
    /// a handle to the file, parses it and loads functions/instructions,
    /// and maps objects.
    ///
    /// # Arguments
    ///
    /// * `path` - The path to the file. This function _does not_ validate the extension.
    ///
    /// # Errors
    pub fn open(path: Rc<String>) -> Result<Rc<Self>> {
        let new_uninit = Rc::new(Self {
            path,
            functions: None,
        });

        let functions = Self::get_functions(&new_uninit)?;

        rc_to_ref(&new_uninit).functions = Some(functions);

        Ok(new_uninit)
    }

    pub fn new(functions: Functions, path: String) -> Rc<Self> {
        Rc::new(Self {
            functions: Some(functions),
            path: Rc::new(path),
        })
    }

    /// Searches for a function given its name.
    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        // let name = format!("{}#{name}", self.path);

        let function = self.functions.as_mut().unwrap().get_mut(&name.to_owned());

        function.ok()
    }

    /// Get the path of the file.
    pub fn path(&self) -> &String {
        &self.path
    }

    /// Get a shared reference to the underlying path pointer.
    pub fn path_shared(&self) -> Rc<String> {
        self.path.clone()
    }

    /// Get the functions associated with an object mapped as `name`.
    pub fn get_object_functions<'a>(
        &'a mut self,
        name: &'a String,
    ) -> Result<impl Iterator<Item = &Function>> {
        let Some(ref mut functions) = self.functions else {
            bail!("no functions")
        };

        Ok(functions.get_object_functions(name))
    }

    /// Get all the functions associated with this file.
    ///
    /// # Errors
    /// If this function encounters any fatal I/O error, an error
    /// variant will be returned. Can also error if a function that
    /// the parser encounters has a name that's non-UTF-8. Errors in
    /// a function's layout are also problematic.
    fn get_functions(rc_of_self: &Rc<Self>) -> Result<Functions> {
        let path = rc_of_self.path();
        let mut reader = BufReader::new(
            File::open(path).with_context(|| format!("failed opening file `{path}`"))?,
        );
        let mut buffer = Vec::new();

        let mut in_function = false;

        // when the loader reaches a `e\0` symbol, all the current function's
        // instructions and name are pushed to this map.
        let mut functions: HashMap<Rc<String>, Function> = HashMap::new();

        // growable list of all the instructions that belong to a function.
        let mut instruction_buffer: Vec<Instruction> = Vec::new();

        // store the current name of the function.
        let mut current_function_name: Option<String> = None;

        while let Ok(size) = reader.read_until(0x00, &mut buffer) {
            if size == 0 {
                break;
            }

            match &buffer[..] {
                // function label syntax: `f function_name_utf8\0`
                [b'f', b' ', name @ .., 0x00] if !in_function => {
                    current_function_name = Some(String::from_utf8(name.to_vec())?);
                    in_function = true;
                }
                // end function syntax: `e\0`
                [b'e', 0x00] | [b'e', .., 0x00] if in_function => {
                    in_function = false;

                    let current_function_name = current_function_name
                        .take()
                        .context("found `end` outside of a function")?;

                    let function = Function::new(
                        Rc::downgrade(rc_of_self),
                        current_function_name,
                        instruction_buffer.into_boxed_slice(),
                    );
                    functions.insert(Rc::new(function.get_qualified_name()), function);
                    instruction_buffer = Vec::new();
                }
                // instruction (w. Args) syntax: `{id} arg1 arg2 "multi arg" arg4\0`
                // where {id}: u8
                [instruction, b' ', args @ .., 0x00] if in_function => {
                    let args = split_string(String::from_utf8_lossy(args))?;

                    let instruction = Instruction::new(*instruction, args);

                    instruction_buffer.push(instruction);
                }
                // instruction (w/O. Args) syntax: `{id}\0`
                [instruction, 0x00] if in_function => {
                    let instruction = Instruction::new(*instruction, Box::new([]));

                    instruction_buffer.push(instruction);
                }
                // Error
                bytes => {
                    let pos = reader.stream_position()?;

                    // Shows the bytes being read that cause the error.
                    panic!("{} {bytes:?} @ {pos}", String::from_utf8_lossy(bytes),)
                }
            }

            buffer.clear();
        }

        Ok(Functions::new(functions))
    }
}
