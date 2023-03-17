use std::cell::Cell;
use std::path::Path;
use std::sync::Arc;
use std::{cell::RefCell, collections::HashMap};

use anyhow::{bail, Result};

use super::function::ReturnValue;
use super::instruction::JumpRequest;
use super::{MScriptFile, Stack};

pub struct Program {
    entrypoint: Arc<String>,
    files_in_use: HashMap<String, Arc<RefCell<MScriptFile>>>,
}

impl Program {
    pub fn new<T>(path: T) -> Result<Self>
    where
        T: AsRef<Path> + ToString,
    {
        let path = path.to_string();
        let main_file = MScriptFile::open(&path)?;
        let entrypoint = Arc::new(path.clone());
        let mut files_in_use = HashMap::with_capacity(1);
        files_in_use.insert(path, main_file);

        Ok(Self {
            entrypoint: entrypoint,
            files_in_use,
        })
    }

    pub fn is_file_loaded(&self, path: &String) -> Option<&Arc<RefCell<MScriptFile>>> {
        self.files_in_use.get(path)
    }

    pub fn add_file(&mut self, path: &String) -> Result<Option<()>> {
        if self.files_in_use.contains_key(path) {
            return Ok(None);
            // bail!("file has already been loaded")
        }

        let new_file = MScriptFile::open(path)?;

        self.files_in_use.insert(path.to_string(), new_file);

        Ok(Some(()))
    }

    pub fn get_file(
        self_cell: Arc<Cell<Self>>,
        path: &String,
    ) -> Result<&Arc<RefCell<MScriptFile>>> {
        unsafe {
            let Some(file) = (*self_cell.as_ptr()).is_file_loaded(path) else {
            bail!("file is not loaded")
        };
            Ok(file)
        }
    }

    fn process_jump_request(arc_of_self: Arc<Cell<Self>>, request: JumpRequest) -> Result<ReturnValue> {
        let mut last_hash = 0;

        for (idx, char) in request.destination_label.chars().enumerate() {
            if char == '#' {
                last_hash = idx;
            }
        }

        if last_hash == 0 || last_hash == request.destination_label.len() - 1 {
            bail!("invalid path (syntax: /path/to/file#function_name");
        }

        let (path, symbol) = request.destination_label.split_at(last_hash);

        let path = path.to_string();
        let symbol = &symbol[1..];

        unsafe {
            (*arc_of_self.as_ptr()).add_file(&path)?;
        }

        let file = Self::get_file(arc_of_self.clone(), &path)?; // arc_of_self.get_mut().get_file(&path)?;

        let return_value = unsafe {
            (*file.as_ptr()).run_function(symbol, request.stack, &mut |req| {
                Self::process_jump_request(arc_of_self.clone(), req)
            })?
        };

        Ok(return_value)
    }

    pub fn execute(self) -> Result<()> {
        let arc_of_self = Arc::new(Cell::new(self));

        let path = unsafe { &(*arc_of_self.as_ptr()).entrypoint };

        let entrypoint = Self::get_file(arc_of_self.clone(), &path)?;
        let stack = Arc::new(Cell::new(Stack::new()));

        // We have to use pointers to get around a guaranteed dynamic thread panic,
        // since `entrypoint` will not have been dropped during subsequent calls to
        // borrow_mut().
        unsafe {
            (*entrypoint.as_ptr()).run_function("main", stack, &mut |req| {
                Self::process_jump_request(arc_of_self.clone(), req)
            })?;
        }

        Ok(())
    }
}
