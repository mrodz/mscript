use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::ops::Index;
use std::path::Path;
use std::sync::Arc;

use crate::bytecode::instruction::{self, run_instruction, Ctx};

use super::attributes_parser::{parse_attributes, Attributes};

use anyhow::{bail, Context, Result};

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

    #[inline]
    pub fn get_qualified_name(&self) -> String {
        format!("{}#{}", self.location, self.name)
    }

    pub fn run(&self) -> Result<Vec<String>> {
        let mut reader = BufReader::new(&*self.location_handle);

        let Ok(pos) = reader.seek(SeekFrom::Start(self.seek_pos)) else {
            bail!("could not get current file position")
        };

        assert_eq!(pos, self.seek_pos);

        let mut lines = vec![];

        let mut context = Ctx::new(&self);

        let mut line_number = self.line_number + 2;

        for line in reader.lines() {
            if line.is_err() {
                bail!("could not read function:\n\t{}", self)
            }

            let line = line.unwrap();

            if line == "end" {
                break;
            }

            let instruction = instruction::parse_line(&line).context("failed parsing line")?;

            run_instruction(&mut context, &instruction)
                .with_context(|| format!("failed to run instruction (L:{line_number})"))?;

            lines.push(line);

            line_number += 1;
        }

        Ok(lines)
    }
}

pub fn open_file<T>(path: T) -> Result<(Arc<T>, Arc<File>)>
where
    T: Location + Clone + 'static,
{
    let path = Arc::new(path);
    let file = Arc::new(File::open(&*path).context("Failed opening file")?);

    Ok((path, file))
}

pub struct Functions {
    map: HashMap<String, Function>,
}

impl Functions {
    pub fn get(&self, signature: &str) -> Result<&Function> {
        let Some(result) = self.map.get(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }
}

impl<'a> Index<&'a str> for Functions {
    type Output = Function;
    fn index(&self, index: &'a str) -> &Self::Output {
        self.map.get(index).unwrap()
    }
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

    Ok(Functions { map: functions })
}
