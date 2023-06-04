use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, SeekFrom};
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use crate::function::Function;
use crate::instruction::{split_string, Instruction};

use super::arc_to_ref;
use super::function::Functions;

#[derive(Debug)]
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
        let name = format!("{}#{name}", self.path);

        let function = self.functions.as_mut().unwrap().get_mut(&name);

        function.ok()
    }

    pub fn get_object_functions<'a, 'b: 'a>(
        &'a mut self,
        name: &'b String,
    ) -> Result<impl Iterator<Item = &Function> + 'a> {
        let Some(ref mut functions) = self.functions else {
            bail!("no functions")
        };

        Ok(functions.get_object_functions(name))
    }

    fn get_functions(arc_of_self: &Arc<Self>) -> Result<Functions> {
        let mut reader = BufReader::new(arc_of_self.handle.as_ref());
        let mut buffer = Vec::new();

        let mut in_function = false;

        let mut functions: HashMap<Arc<String>, Function> = HashMap::new();
        let mut instruction_buffer: Vec<Instruction> = Vec::new();
        let mut current_function_name: Option<String> = None;

        while let Ok(size) = reader.read_until(0x00, &mut buffer) {
            if size == 0 {
                #[cfg(feature = "developer")]
                println!("EOF @ L:{line_number}");

                break;
            }

            match &buffer[..] {
                [b'f', b' ', name @ .., 0x00] if !in_function => {
                    current_function_name = Some(String::from_utf8(name.to_vec())?);
                    in_function = true;
                }
                [b'e', 0x00] | [b'e', .., 0x00] if in_function => {
                    in_function = false;
                    if current_function_name.is_none() {
                        bail!("found `end` outside of a function")
                    }

                    let current_function_name = current_function_name.take().unwrap();

                    let function = Function::new(
                        arc_of_self.clone(),
                        current_function_name,
                        instruction_buffer.into_boxed_slice(),
                    );
                    functions.insert(Arc::new(function.get_qualified_name()), function);
                    instruction_buffer = Vec::new();
                }
                [instruction, b' ', args @ .., 0x00] if in_function => {
                    let args = split_string(String::from_utf8_lossy(args))?;

                    let instruction = Instruction::new(*instruction, args);

                    instruction_buffer.push(instruction);
                }
                [instruction, 0x00] if in_function => {
                    let instruction = Instruction::new(*instruction, Box::new([]));

                    instruction_buffer.push(instruction);
                }
                bytes => {
                    let pos = reader.stream_position()?;
                    panic!(
                        "{} {bytes:?} @ {pos}",
                        bytes.iter().map(|x| *x as char).collect::<String>()
                    )
                }
            }

            buffer.clear();
        }

        Ok(Functions { map: functions })
    }
}

impl MScriptFile {
    pub fn open(path: &String) -> Result<Arc<Self>> {
        let new_uninit = Arc::new(Self {
            handle: Arc::new(
                File::open(path).with_context(|| format!("failed opening file `{path}`"))?,
            ),
            path: Arc::new(path.clone()),
            functions: None,
        });

        let functions = Self::get_functions(&new_uninit)?;

        arc_to_ref(&new_uninit).functions = Some(functions);

        Ok(new_uninit)
    }
}
