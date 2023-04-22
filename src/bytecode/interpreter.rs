use std::path::Path;
use std::sync::Arc;
use std::collections::HashMap;

use anyhow::{bail, Result};

use super::function::ReturnValue;
use super::instruction::JumpRequest;
use super::{arc_to_ref, MScriptFile, Stack};

pub struct Program {
    entrypoint: Arc<String>,
    files_in_use: HashMap<String, Arc<MScriptFile>>,
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

    pub fn is_file_loaded(&self, path: &String) -> Option<Arc<MScriptFile>> {
        self.files_in_use.get(path).map(|arc| arc.clone())
    }

    pub fn add_file(&mut self, path: &String) -> Result<Option<()>> {
        if self.files_in_use.contains_key(path) {
            return Ok(None);
        }

        let new_file = MScriptFile::open(path)?;

        self.files_in_use.insert(path.to_string(), new_file);

        Ok(Some(()))
    }

    pub fn get_file(self_cell: Arc<Self>, path: &String) -> Result<Arc<MScriptFile>> {
        let Some(file) = self_cell.is_file_loaded(path) else {
                bail!("file is not loaded")
            };

        Ok(file)
    }

    fn process_jump_request(arc_of_self: Arc<Self>, request: JumpRequest) -> Result<ReturnValue> {
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

        arc_to_ref(&arc_of_self).add_file(&path);

        let file = Self::get_file(arc_of_self.clone(), &path)?;

        let Some(function) = arc_to_ref(&file).get_function(symbol) else {
            bail!("could not find function (missing `{symbol}`)")
        };

        let return_value = function.run(
            request.arguments,
            Arc::clone(&request.stack),
            request.callback_state,
            &mut |req| Self::process_jump_request(arc_of_self.clone(), req),
        )?;

        Ok(return_value)
    }

    pub fn execute(self) -> Result<()> {
        let arc_of_self = Arc::new(self);

        let path = &*arc_of_self.entrypoint;

        let entrypoint = Self::get_file(arc_of_self.clone(), path)?;
        let stack = Arc::new(Stack::new());

        // We have to use pointers to get around a guaranteed dynamic thread panic,
        // since `entrypoint` will not have been dropped during subsequent calls to
        // borrow_mut().
        let Some(function) = arc_to_ref(&entrypoint).get_function("main") else {
            bail!("could not find entrypoint (hint: try adding `function main`)")
        };

        function.run(vec![], stack, None, &mut |req| {
            Self::process_jump_request(arc_of_self.clone(), req)
        })?;

        Ok(())
    }
}
