use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::ops::{Index, IndexMut};
use std::{sync::Arc, fs::File};

use anyhow::{bail, Result, Context};

use crate::bytecode::instruction;

use super::attributes_parser::Attributes;
use super::instruction::{Ctx, Instruction, run_instruction};
use super::variable::Primitive;
use super::interpreter::Location;

pub struct Function {
    location: Arc<dyn Location>,
    location_handle: Arc<File>,
    line_number: u32,
    seek_pos: u64,
    attributes: Vec<Attributes>,
    name: String,
}

#[derive(Debug)]
pub struct ReturnValue(Option<Primitive>);
impl ReturnValue {
    pub fn get(&self) -> &Option<Primitive> {
        &self.0
    }
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

    fn run_and_ret<'a>(
        context: &'a mut Ctx,
        instruction: &Instruction,
    ) -> Result<Option<&'a Option<Primitive>>> {
        run_instruction(context, instruction)
            .with_context(|| format!("failed to run instruction"))?;

        if let Ok(ret) = context.get_return_value() {
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    pub fn run(&mut self) -> Result<ReturnValue> {
        let handle = &*self.location_handle.clone();
        let mut reader = BufReader::new(handle);

        let Ok(pos) = reader.seek(SeekFrom::Start(self.seek_pos)) else {
            bail!("could not get current file position")
        };

        assert_eq!(pos, self.seek_pos);

        let mut context = Ctx::new(&self);

        let mut line_number = self.line_number + 2;

        let lines = reader.lines();

        for line in lines {
            if line.is_err() {
                bail!("could not read function:\n\t{}", self)
            }

            let line = line.unwrap();

            if line == "end" {
                break;
            }

            let instruction = instruction::parse_line(&line).context("failed parsing line")?;

            let ret = Self::run_and_ret(&mut context, &instruction)
                    .with_context(|| format!("line {line_number}"))?;

            if let Some(primitive) = ret {
                return Ok(ReturnValue(primitive.clone()))
            }

            line_number += 1;
        }

        Ok(ReturnValue(None))
    }
}

pub struct Functions(pub HashMap<String, Function>);

impl Functions {
    pub fn get(&self, signature: &str) -> Result<&Function> {
        let Some(result) = self.0.get(signature) else {
            bail!("unknown function ({signature})");
        };

        Ok(result)
    }
}

impl<'a> Index<&'a str> for Functions {
    type Output = Function;
    fn index(&self, index: &'a str) -> &Self::Output {
        self.0.get(index).unwrap()
    }
}

impl<'a> IndexMut<&'a str> for Functions {
    fn index_mut(&mut self, index: &'a str) -> &mut Self::Output {
        self.0.get_mut(index).unwrap()
    }
}