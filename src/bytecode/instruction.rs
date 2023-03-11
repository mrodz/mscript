use std::collections::HashSet;

use anyhow::{bail, Result};

use super::{interpreter::Function, variable::Primitive};

pub type InstructionSignature = fn(&mut Ctx<'_>, &Vec<String>) -> ();

mod implementations {
    use crate::bytecode::variable::Primitive;

    use super::Ctx;

    pub(crate) fn constexpr(ctx: &mut Ctx, args: &Vec<String>) {
        assert!(args.len() == 1);

        let var = Primitive::from(&args[0]);

        ctx.push(var);
    }

    pub(crate) fn pop(ctx: &mut Ctx, args: &Vec<String>) {
        assert!(args.len() == 0);

        ctx.pop();
    }

    pub(crate) fn stack_dump(ctx: &mut Ctx, args: &Vec<String>) {
        println!("====== Start Context Dump ======");
        'get_data: {
            if let Some(arg0) = args.first() {
                if arg0 == "verbose" {
                    dbg!(ctx);
                    break 'get_data;
                } else {
                    panic!("unknown debug argument ({arg0})")
                }
            }

            println!("Function: {}", ctx.function);
            println!("Accessible Symbols: {:?}", ctx.names);
            println!("Stack: {:#?}", ctx.stack);
        }
        println!("======= End Context Dump =======");
    }
}

pub fn query(name: &String) -> InstructionSignature {
    match name.as_str() {
        "constexpr" => implementations::constexpr,
        "stack_dump" => implementations::stack_dump,
        "pop" => implementations::pop,
        _ => unreachable!("unknown bytecode instruction ({name})"),
    }
}

pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) {
    query(&instruction.name)(ctx, &instruction.arguments)
}

pub fn split_string(string: &String) -> Result<Vec<String>> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes {
            if char == ' ' || char == ',' {
                if buf.len() != 0 {
                    result.push(buf.to_string());
                    buf.clear();
                }
                continue;
            }
        }

        match char {
            '\\' => {
                if escaping {
                    buf.push(char);
                }
                escaping = !escaping;
                continue;
            }
            ',' => {
                if escaping {
                    buf.push(char);
                    escaping = !escaping;
                }
                continue;
            }
            '"' => {
                buf.push(char);

                if escaping {
                    escaping = false;
                    continue;
                }

                if in_quotes {
                    result.push(buf.to_string());
                    buf.clear();
                }

                in_quotes = !in_quotes;
                continue;
            }
            'n' if escaping => {
                buf.push('\n');
                escaping = false;
                continue;
            }
            'r' if escaping => {
                buf.push('\r');
                escaping = false;
                continue;
            }
            't' if escaping => {
                buf.push('\t');
                escaping = false;
                continue;
            }
            _ => {
                if escaping {
                    bail!("Unknown escape sequence: \\{}", char)
                }
            }
        }

        buf.push(char);
    }

    if in_quotes {
        bail!("Found EOL while parsing string: {string}")
    } else {
        if buf.len() > 0 {
            result.push(buf.to_string());
        }
        Ok(result)
    }
}

pub fn parse_line(line: &String) -> Result<Instruction> {
    let mut arguments = split_string(line)?;

    assert!(arguments.len() >= 1);

    let name = arguments.remove(0);

    let mut tabs = 0;

    let bytes = name.as_bytes();

    while bytes[tabs] == b'\t' {
        tabs += 1;
    }

    Ok(Instruction {
        name: name.trim_start_matches('\t').into(),
        arguments,
        children: vec![],
        tab_level: tabs as u8,
    })
}

#[derive(Debug)]
pub struct Ctx<'a> {
    stack: Vec<Primitive>,
    names: HashSet<&'a String>,
    function: &'a Function,
}

impl<'a> Ctx<'a> {
    pub fn new(function: &'a Function) -> Self {
        Self {
            stack: vec![],
            names: HashSet::new(),
            function,
        }
    }

    fn register_variable(&'a mut self, name: &'a String) {
        self.names.insert(name);
    }

    fn push(&mut self, var: Primitive) {
        self.stack.push(var);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }
}

pub struct Instruction {
    name: String,
    arguments: Vec<String>,
    children: Vec<Instruction>,
    tab_level: u8,
}
