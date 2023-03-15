use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::{BufRead, BufReader, Lines, Seek, SeekFrom};
use std::path::Path;
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::bytecode::attributes_parser::{parse_attributes, Attributes};
use crate::bytecode::function::Function;

use super::function::{Functions, ReturnValue};
use super::Stack;

pub trait Location: AsRef<Path> + Display + Debug {}

impl Location for &str {}
impl Location for String {}

pub struct MScriptFile<T>
where
    T: Location + 'static,
{
    path: Arc<T>,
    handle: Arc<File>,
    functions: Functions,
}

impl<T> MScriptFile<T>
where
    T: Location + 'static,
{
    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        let name = format!("{}#{name}", self.path.to_string());
        dbg!(&name);

        let function = self.functions.get_mut(&name);
        return function.map_or_else(|_| None, |ok| Some(ok));
    }

    pub fn run_function(&mut self, name: &str, stack: &mut Stack) -> Result<ReturnValue> {
        let Some(function) = self.get_function(name) else {
			bail!("could not find function (missing `{name}`)")
		};

        function.run(stack)
    }

    pub fn open(path: T) -> Result<Self> {
        let path = Arc::new(path);
        let handle = Arc::new(File::open(&*path).context("failed opening file")?);
        let functions = Self::get_functions(&handle, &path)?;

        Ok(Self {
            path: Arc::clone(&path),
            handle: Arc::clone(&handle),
            functions,
        })
    }

    fn get_functions(file: &Arc<File>, path: &Arc<T>) -> Result<Functions> {
        println!("Functions in {path}");
        let mut reader = BufReader::new(&**file);
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
                        path.clone(),
                        file.clone(),
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
