use anyhow::{bail, Result};
use bytecode::compilation_lookups::*;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::borrow::Cow;
use std::fs::File;
use std::io::{BufRead, BufReader, Seek, Write};
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

pub fn is_instruction_deprecated(name: &str) -> bool {
    matches!(name, "nop" | "char" | "endif")
}

pub const TRANSPILED_SOURCE_FILE_EXTENSION: &str = ".transpiled.mmm";

pub fn is_path_a_transpiled_source(path: &String) -> bool {
    fn ends_with_ignore_case(string: &str, pat: &str) -> bool {
        for (c1, c2) in string.chars().rev().zip(pat.chars().rev()) {
            if !c1.eq_ignore_ascii_case(&c2) {
                return false
            } 
        }

        true
    }

    ends_with_ignore_case(&path, TRANSPILED_SOURCE_FILE_EXTENSION)
}

pub fn transpile_file(path: &str, new_path: &str) -> Result<()> {
    let path = Path::new(&path);

    let file = File::open(&path)?;
    // let spinner_style = ProgressStyle::with_template("{prefix:.bold.dim} {spinner} {wide_msg}")?;
    let functions = MultiProgress::new();

    let pb = functions.add(ProgressBar::new(file.metadata()?.len()));
    pb.set_style(ProgressStyle::with_template("{prefix:.bold.dim} [{elapsed_precise}] {wide_bar:.cyan/blue} {human_pos:>7}/{human_len:7} bytes")?);
    pb.set_prefix("Transpiling:");

    let spinner_style = ProgressStyle::with_template("{prefix:.bold.dim} {spinner} {wide_msg}")
        .unwrap()
        .tick_chars("⠁⠂⠄⡀⢀⠠⠐⠈ ");

    let mut reader = BufReader::new(file);
    let mut buffer = String::new();

    let mut new_file = File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(new_path.clone())?;

    let mut instruction_buffer: Vec<Instruction> = Vec::new();
    let mut current_function_name: Option<String> = None;
    let mut current_function_pb: Option<ProgressBar> = None;

    while let Ok(size) = reader.read_line(&mut buffer) {
        if size == 0 {
            break;
        }

        let pos = reader.stream_position()?;
        pb.set_position(pos);

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

                let pb = functions.add(ProgressBar::new_spinner());
                pb.set_style(spinner_style.clone());
                pb.set_message(name.clone());

                current_function_pb = Some(pb);
                
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

            write!(new_file, "f {}\0{body}e\0", function.name)?;

            instruction_buffer = Vec::new();

            if let Some(ref pb) = current_function_pb {
                pb.finish_with_message(format!("finished {current_function_name}"));
            }
        } else {
            let whitespace_parts = buffer.split_once(' ');

            if let Some((name, args)) = whitespace_parts {
                if let Some(ref pb) = current_function_pb {
                    pb.tick();
                }

                let arguments = split_string(Cow::Borrowed(args))?;

                let name = name.trim_start();

                if is_instruction_deprecated(name) {
                    bail!("{name:?} is deprecated and not supported by this interpreter")
                }

                let Some(instruction_in_byte_fmt) = string_instruction_representation_to_byte(name) else {
                    bail!("{name} is not a valid instruction")
                };

                let instruction = Instruction {
                    arguments,
                    name: *instruction_in_byte_fmt,
                };

                instruction_buffer.push(instruction);
            } else {
                let instruction = trimmed_end;

                if is_instruction_deprecated(trimmed_end) {
                    bail!("{trimmed_end:?} is deprecated and not supported by this interpreter")
                }

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

    pb.finish();

    let meta = new_file.metadata()?;

    println!("Success! Wrote {} bytes to {}", meta.len(), new_path);

    Ok(())
}
