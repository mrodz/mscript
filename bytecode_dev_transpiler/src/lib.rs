use anyhow::{bail, Result};
use bytecode::compilation_lookups::*;
use std::borrow::Cow;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::path::Path;
use std::sync::Arc;

#[derive(Debug)]
pub struct Instruction {
    pub name: u8,
    arguments: Box<[String]>,
}

impl Instruction {
    fn repr(&self) -> String {
        let mut args = String::new();

        if self.arguments.len() >= 1 {
            for arg in &self.arguments[..] {
                args.push(' ');
                if arg.contains(' ') {
                    args.push('\"');
                    args.push_str(&arg);
                    args.push('\"');
                } else {
                    args.push_str(&arg);
                }
            }
        }

        format!("{}{}\0", self.name as char, args)
    }
}

#[derive(Debug)]
struct Function {
    pub(crate) instructions: Box<[Instruction]>,
    pub(crate) name: Arc<String>,
}

pub fn transpile_file(path: String) -> Result<()> {
    let path = Path::new(&path);
    let new_path = path.with_extension("").with_extension("mmm");

    let file = File::open(&path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = String::new();

    let mut new_file = File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(new_path)?;
    // new_file.write_all(result.as_bytes())?;

    // let mut functions: HashMap<Arc<String>, Function> = HashMap::new();

    let mut instruction_buffer: Vec<Instruction> = Vec::new();
    let mut current_function_name: Option<String> = None;

    while let Ok(size) = reader.read_line(&mut buffer) {
        if size == 0 {
            break;
        }

        let bytes = buffer.as_bytes();

        let trimmed_end = buffer.trim();

        if trimmed_end.len() == 0 {
            buffer.clear();
            continue;
        }

        if bytes.len() >= 9 {
            let parts = bytes.split_at(8);
            if let (b"function", [b' ', name @ ..]) = parts {
                let name = String::from_utf8_lossy(name).trim_end().to_string();

                current_function_name = Some(name);

                buffer.clear();
                continue;
            }
        }

        if "end" == trimmed_end {
            if current_function_name.is_none() {
                bail!("found `end` outside of a function")
            }

            let current_function_name = Arc::new(current_function_name.take().unwrap());

            let function = Function {
                name: current_function_name.clone(),
                instructions: instruction_buffer.into_boxed_slice(),
            };

            let mut body = String::new();
            for instruction in &function.instructions[..] {
                body.push_str(&instruction.repr());
            }
            // result.push_str(&format!("f {}\0{body}e\0", function.name))
            write!(new_file, "f {}\0{body}e\0", function.name)?;
            // new_file.write_fmt(fmt) .write_all(buf)?;
            // functions.insert(current_function_name, function);

            // functions.insert(Arc::new(function.get_qualified_name()), function);
            instruction_buffer = Vec::new();
        } else {
            let whitespace_parts = buffer.split_once(' ');

            if let Some((name, args)) = whitespace_parts {
                let arguments = split_string(Cow::Borrowed(args))?;

                let Some(instruction_in_byte_fmt) = string_instruction_representation_to_byte(name.trim_start()) else {
					bail!("{name} is not a valid instruction")
				};

                let instruction = Instruction {
                    arguments,
                    name: *instruction_in_byte_fmt,
                };

                instruction_buffer.push(instruction);
            } else {
                let instruction = trimmed_end;

                let Some(instruction_in_byte_fmt) = string_instruction_representation_to_byte(&instruction) else {
					bail!("{instruction} is not a valid instruction")
				};

                instruction_buffer.push(Instruction {
                    name: *instruction_in_byte_fmt,
                    arguments: Box::new([]),
                })
            }
        }

        buffer.clear();
    }

    // let mut result = String::new();
    // for function in functions.values() {
    // 	let mut body = String::new();
    // 	for instruction in &function.instructions[..] {
    // 		body.push_str(&instruction.repr(path, &new_path));
    // 	}
    // 	result.push_str(&format!("f {}\0{body}e\0", function.name))
    // }

    // println!("{}", result);

    Ok(())
}
