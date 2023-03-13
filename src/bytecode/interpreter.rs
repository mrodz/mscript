use std::collections::HashMap;
use std::fmt::Display;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::path::Path;
use std::sync::Arc;

use super::attributes_parser::{parse_attributes, Attributes};
use super::function::{Function, Functions};

use anyhow::{bail, Context, Result};

pub trait Location: AsRef<Path> + Display {}

impl Location for &str {}
impl Location for String {}

pub fn open_file<T>(path: T) -> Result<(Arc<T>, Arc<File>)>
where
    T: Location + Clone + 'static,
{
    let path = Arc::new(path);
    let file = Arc::new(File::open(&*path).context("Failed opening file")?);

    Ok((path, file))
}

pub fn functions<T>(file: Arc<File>, path: Arc<T>) -> Result<Functions>
where
    T: Location + Clone + 'static,
{
    println!("Functions in {path}");
    let mut reader = BufReader::new(&*file);
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
