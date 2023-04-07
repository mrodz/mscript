use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::bytecode::attributes_parser::{parse_attributes, Attributes};
use crate::bytecode::function::Function;

use super::function::Functions;

pub struct MScriptFile {
    pub(crate) path: Arc<String>,
    pub(crate) handle: Arc<File>,
    functions: Option<Functions>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IfStatement {
    If(u64, Box<IfStatement>),
    Else(u64, Box<IfStatement>),
    EndIf(u64),
}

impl IfStatement {
    pub fn next_pos(&self) -> &u64 {
        use IfStatement::*;

        match self {
            If(_, box Else(pos, _)) => pos,
            If(_, box EndIf(pos)) => pos,
            Else(_, box EndIf(pos)) => pos,
            EndIf(_) => panic!("endif does not have a `next` attribute"),
            _ => unreachable!("{self:?}"),
        }
    }
}

/// This is an expensive call. Only call for errors or non-build debugging.
pub fn get_line_number_from_pos(reader: &mut BufReader<&File>, pos: u64) -> Result<u64> {
    let old_pos = reader.stream_position()?;
    reader.seek(SeekFrom::Start(0))?;

    let mut result = 0;

    let mut buffer = String::new();

    loop {
        reader.read_line(&mut buffer)?;

        result += 1;

        if reader.stream_position()? >= pos {
            break;
        }
    }

    // reset
    reader.seek(SeekFrom::Start(old_pos))?;

    Ok(result)
}

impl MScriptFile {
    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        let name = format!("{}#{name}", self.path.to_string());
        let function = self.functions.as_mut().unwrap().get_mut(&name);

        return function.map_or_else(|_| None, |ok| Some(ok));
    }

    pub fn get_if_from(&self, if_pos: u64) -> Option<IfStatement> {
        let Some(ref functions) = self.functions else {
            panic!("file does not have functions");
        };

        functions.if_mapper.get(&if_pos).map(|x| x.clone())
    }

    pub fn get_object_functions<'a, 'b: 'a>(&'a mut self, name: &'b String) -> Result<impl Iterator<Item = &Function> + 'a> {
        let Some(ref mut functions) = self.functions else {
            bail!("no functions")
        };

        Ok(functions.get_object_functions(name))
    }

    fn get_functions(arc_of_self: &Arc<RefCell<Self>>) -> Result<Functions> {
        println!("Functions in {}", arc_of_self.borrow().path);

        let handle_ref = arc_of_self.borrow();
        let mut reader = BufReader::new(handle_ref.handle.as_ref());
        let mut buffer = String::new();

        let mut line_number: u32 = 0;

        let mut functions: HashMap<String, Function> = HashMap::new();

        let mut current_attributes: Vec<Attributes> = vec![];

        let mut if_mapper: HashMap<u64, IfStatement> = HashMap::new();

        let mut if_positions: Vec<u64> = Vec::new();
        let mut else_to_if_mapper: HashMap<u64, u64> = HashMap::new();

        while let Ok(size) = reader.read_line(&mut buffer) {
            if size == 0 {
                println!("EOF @ L:{line_number}");
                break;
            }

            let seek_pos = reader.stream_position()?;

            if buffer.starts_with("#[") {
                let attr = parse_attributes(&buffer).context("Failed parsing attributes")?;
                current_attributes.push(attr);
            } else {
                let mut parts = buffer.split_ascii_whitespace();
                let (first, second) = (parts.next(), parts.next());

                match (first, second) {
                    (Some("function"), Some(name)) => {
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
                    (Some("if"), None) => {
                        if_positions.push(seek_pos);
                    }
                    (Some("else"), None) => {
                        else_to_if_mapper
                            .insert(*if_positions.last().expect("else without if"), seek_pos);
                    }
                    (Some("endif"), None) => {
                        use IfStatement::*;

                        let if_pos = if_positions.pop().expect("endif without if");

                        let else_pos = else_to_if_mapper.get(&if_pos);

                        let endif_part = Box::new(EndIf(seek_pos));

                        let if_part = If(
                            if_pos,
                            if let Some(else_pos) = else_pos {
                                Box::new(Else(*else_pos, endif_part))
                            } else {
                                endif_part
                            },
                        );

                        if_mapper.insert(if_pos, if_part);
                    }
                    _ => (),
                }
            }

            buffer.clear();

            line_number += 1;
        }

        if if_positions.len() != 0 {
            bail!(
                "reached end of file without closing if at {:#x} (use endif)",
                if_positions.first().unwrap()
            )
        }

        Ok(Functions {
            map: functions,
            if_mapper,
        })
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

        new_uninit.borrow_mut().functions = Some(functions);

        Ok(new_uninit)
    }
}
