use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::bytecode::attributes_parser::{parse_attributes, Attributes};
use crate::bytecode::function::Function;

use super::function::{Functions, ReturnValue};
use super::instruction::JumpRequest;
use super::Stack;

// pub trait Location: AsRef<Path> + Display + Debug {}

pub struct MScriptFile {
    pub(crate) path: Arc<String>,
    pub(crate) handle: Arc<File>,
    functions: Option<Functions>,
}

impl MScriptFile {
    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        let name = format!("{}#{name}", self.path.to_string());
        let function = self.functions.as_mut().unwrap().get_mut(&name);
        //?.get_mut(&name);

        return function.map_or_else(|_| None, |ok| Some(ok));
    }

    pub fn run_function(
        &mut self,
        name: &str,
        stack: Arc<Cell<Stack>>,
        jump_callback: &mut impl FnMut(JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        let Some(function) = self.get_function(name) else {
			bail!("could not find function (missing `{name}`)")
		};

        function.run(stack, jump_callback)
    }

    fn get_functions(arc_of_self: &Arc<RefCell<Self>>) -> Result<Functions> {
        println!("Functions in {}", arc_of_self.borrow().path);

        let handle_ref = arc_of_self.borrow();
        let mut reader = BufReader::new(handle_ref.handle.as_ref());
        let mut buffer = String::new();

        let mut line_number: u32 = 0;

        let mut functions: HashMap<String, Function> = HashMap::new();

        let mut current_attributes: Vec<Attributes> = vec![];

        while let Ok(size) = reader.read_line(&mut buffer) {
            if size == 0 {
                println!("EOF @ L:{line_number}");
                break;
            }

            let Ok(seek_pos) = reader.seek(SeekFrom::Current(0)) else {
				bail!("could not get current file position")
			};

            if buffer.starts_with("#[") {
                let attr = parse_attributes(&buffer).context("Failed parsing attributes")?;
                current_attributes.push(attr);
            } else {
                let mut parts = buffer.split_ascii_whitespace();
                if let (Some("function"), Some(name)) = (parts.next(), parts.next()) {
                    let function = Function::new(
                        arc_of_self.clone(),
                        line_number,
                        current_attributes,
                        name.to_string(),
                        seek_pos,
                    );

                    println!("\t{function}");

                    functions.insert(function.get_qualified_name(), function);
                    current_attributes = vec![];
                }
            }

            buffer.clear();

            line_number += 1;
        }

        Ok(Functions(functions))
    }
}

impl MScriptFile {
    pub fn open(path: &String) -> Result<Arc<RefCell<Self>>> {
        let new_uninit = Arc::new(RefCell::new(Self {
            handle: Arc::new(File::open(path).context("failed opening file")?),
            path: Arc::new(path.clone()),
            functions: None,
        }));

        let functions = Self::get_functions(&new_uninit)?;

        {
            // scoped to ensure the mutable reference is dropped before returning.
            new_uninit.borrow_mut().functions = Some(functions);
        }

        Ok(new_uninit)
    }
}
