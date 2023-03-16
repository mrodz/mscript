use std::path::Path;
use std::sync::Arc;
use std::{cell::RefCell, collections::HashMap};

use anyhow::{bail, Result};

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

    pub fn get_file(&mut self, path: &String) -> Result<&Arc<RefCell<MScriptFile>>> {
        let Some(file) = self.is_file_loaded(path) else {
            bail!("file is not loaded")
        };

        Ok(file)
    }

    fn process_jump_request(&mut self, request: JumpRequest) -> Result<()> {
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

        self.add_file(&path)?;
        let file = self.get_file(&path)?;

        let mut new_stack = Stack::new();

        new_stack.extend(&"temporary: new stack on fn call".to_string());

        let mut c = |request| {
            dbg!(request);
            Ok(())
        };

        // todo: 
        // - allow more than two functions calls.
        // - pass stack frame to this function
        // - find a way to do this recursively.

        unsafe {
            (*file.as_ptr()).run_function(symbol, &mut new_stack, &mut c)?;
        }

        Ok(())
    }

    pub fn execute(mut self) -> Result<()> {
        let entrypoint = self.get_file(&self.entrypoint.as_ref().clone())?;

        let mut stack = Stack::new();

        // We have to use pointers to get around a guaranteed dynamic thread panic,
        // since `entrypoint` will not have been dropped during subsequent calls to
        // borrow_mut().
        unsafe {
            (*entrypoint.as_ptr()).run_function("main", &mut stack, &mut |request| {
                self.process_jump_request(request)
            })?;
        }

        Ok(())
    }
}
