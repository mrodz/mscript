use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::path::Path;
use std::sync::Arc;

use super::attributes_parser::{parse_attributes, Attributes};
use anyhow::{bail, Result};

pub trait Location: AsRef<Path> + Display {}

impl Location for &str {}
impl Location for String {}
pub struct Function {
    location: Arc<dyn Location>,
    location_handle: Arc<File>,
    line_number: u32,
    seek_pos: u64,
    attributes: Vec<Attributes>,
    name: String,
}



impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function {{ name: {:?}, location: {}, line_number: {}, seek_pos: {}, attributes: {:?} }}", self.name, &*self.location, self.line_number, self.seek_pos, self.attributes)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut attributes = String::new();

        for attribute in &self.attributes {
            attributes.push_str(&(attribute.to_string() + " "))
        }

        write!(
            f,
            "{}@@{}:{}@@ function {} ({:#x})",
            attributes,
            self.location,
            self.line_number + 1,
            self.name,
			self.seek_pos
        )
    }
}

impl Function {
    pub fn new(
        location: Arc<dyn Location>,
        location_handle: Arc<File>,
        line_number: u32,
        attributes: Vec<Attributes>,
        name: String,
        seek_pos: u64,
    ) -> Self {
        Self {
            location,
            location_handle,
            line_number,
            attributes,
            name,
            seek_pos,
        }
    }
}

/// Read all the lines of a function, based on its file location
///
pub fn enter_function(function: &Function) -> Result<()> {
    let mut reader = BufReader::new(&*function.location_handle);

    let Ok(pos) = reader.seek(SeekFrom::Start(function.seek_pos)) else {
		bail!("could not get current file position")
	};

    assert_eq!(pos, function.seek_pos);

    for line in reader.lines() {
        if line.is_err() {
            bail!("Could not read function:\n\t{}", function)
        }

        let line = line.unwrap();

        if line == "end" {
            break;
        }

        println!("{line}")
    }

    Ok(())
}

pub fn open_file<T>(path: T) -> Result<(Arc<T>, Arc<File>)>
where
    T: Location + Clone + 'static,
{
	let path = Arc::new(path);
    let file = Arc::new(File::open(&*path)?);

	Ok((path, file))
}

pub fn functions<T>(file: Arc<File>, path: Arc<T>) -> Result<Vec<Function>>
where
    T: Location + Clone + 'static,
{
	println!("Functions in {path}");
    let mut reader = BufReader::new(&*file);
    let mut buffer = String::new();

    let mut line_number: u32 = 0;

    let mut functions: Vec<Function> = vec![];

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
            let attr = parse_attributes(&buffer)?;
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
				
                functions.push(function);
                current_attributes = vec![];
            }
        }

        buffer.clear();

        line_number += 1;
    }

	println!("===");
    Ok(functions)
}
