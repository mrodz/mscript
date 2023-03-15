use std::collections::HashSet;

use anyhow::{bail, Context, Result};

use super::function::Function;
use super::stack::Stack;
use super::variable::Primitive;

pub type InstructionSignature = fn(&mut Ctx<'_>, &Vec<String>) -> Result<()>;

mod implementations {
    use std::io::{stdin, stdout, Write};

    use anyhow::Context;

    use super::*;
    use crate::bytecode::variable::{bin_op_from, bin_op_result, Primitive};

    pub(crate) fn constexpr(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        if args.len() != 1 {
            bail!("unexpected 1 parameter")
        }

        let var = Primitive::from(&args[0]);

        ctx.push(var);

        Ok(())
    }

    pub(crate) fn pop(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        if args.len() != 0 {
            bail!("unexpected parameter")
        }

        ctx.pop();

        Ok(())
    }

    pub(crate) fn stack_dump(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        println!("====== Start Context Dump ======");
        'get_data: {
            if let Some(arg0) = args.first() {
                if arg0 == "verbose" {
                    dbg!(ctx);
                    break 'get_data;
                } else {
                    bail!("unknown debug argument ({arg0})")
                }
            }

            println!("Function: {}", ctx.function);
            println!("Stack Trace:\n{}", ctx.call_stack);
            println!("Operating Stack: {:#?}", ctx.stack);
        }
        println!("======= End Context Dump =======");

        Ok(())
    }

    pub(crate) fn bin_op(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        let symbols = match args.first() {
            Some(symbols) => symbols,
            None => bail!("Expected an operation [+,-,*,/,%]"),
        }
        .as_bytes();

        let symbol = symbols[0] as char;

        let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
            unreachable!()
        };

        let (i_fn, f_fn) = bin_op_from(symbol).context("constructing bin op")?;

        let result = bin_op_result(left, right, i_fn, f_fn)?;

        ctx.clear_and_set_stack(result);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn nop(_ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
        Ok(())
    }

    pub(crate) fn void(ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
        ctx.stack.clear();

        Ok(())
    }

    pub(crate) fn breakpoint(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        print!("[!!] BREAKPOINT\n[!!] options\n[!!] - continue\n[!!] - dump\n[!!] Enter Option: ");

        let mut buf = String::new();
        stdout().flush()?;
        stdin().read_line(&mut buf)?;

        match buf.trim_end() {
            "continue" => Ok(()),
            "dump" => {
                stack_dump(ctx, args)?;
                Ok(())
            }
            buf => {
                println!("[!!]\n[!!] BREAKPOINT\n[!!] '{buf}' is not a valid option.\n[!!]");
                breakpoint(ctx, args)
            }
        }
    }

    pub(crate) fn ret(ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
        if ctx.stack_size() > 1 {
            bail!("ret can only return a single item");
        }

        let var = ctx.pop();

        ctx.return_now(var);

        Ok(())
    }

    macro_rules! make_type {
        ($name:ident) => {
            pub(crate) fn $name(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
                if args.len() != 1 {
                    bail!("expected 1 parameter")
                }

                let var = Primitive::$name(&args[0])?;

                ctx.push(var);

                Ok(())
            }
        };
    }

    make_type!(make_bool);
    make_type!(make_str);
    make_type!(make_int);
    make_type!(make_float);
    make_type!(make_char);
    make_type!(make_byte);

    pub(crate) fn printn(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
        let Some(arg) = args.first() else {
            bail!("expected 1 parameter of type __rust__::usize, or * to print all");
        };

        if arg == "*" {
            let Some(first) = ctx.stack.first() else {
                return Ok(())
            };

            print!("{first}");
            for var in &ctx.stack[1..] {
                print!(", {var}");
            }

            println!();

            return Ok(());
        }

        let Ok(arg) = usize::from_str_radix(arg, 10) else {
            bail!("expected 1 parameter of type __rust__::usize");
        };

        println!("{:?}", ctx.stack.get(arg));

        Ok(())
    }

    // pub(crate) fn jump(ctx: &mut Ctx, args: &Vec<String>) -> Result<()> {
    //     let Some(first) = args.first() else {
    //         bail!("expected one argument");
    //     };

    //     ctx.request_jump(first);
    // }

}

pub fn query(name: &String) -> InstructionSignature {
    match name.as_str() {
        "constexpr" => implementations::constexpr,
        "stack_dump" => implementations::stack_dump,
        "pop" => implementations::pop,
        "bin_op" => implementations::bin_op,
        "nop" => implementations::nop,
        "bool" => implementations::make_bool,
        "string" => implementations::make_str,
        "int" => implementations::make_int,
        "float" => implementations::make_float,
        "char" => implementations::make_char,
        "byte" => implementations::make_byte,
        "void" => implementations::void,
        "breakpoint" => implementations::breakpoint,
        "ret" => implementations::ret,
        "printn" => implementations::printn,
        _ => unreachable!("unknown bytecode instruction ({name})"),
    }
}

pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) -> Result<()> {
    query(&instruction.name)(ctx, &instruction.arguments)?;

    Ok(())
}

pub fn split_string(string: &String) -> Result<Vec<String>> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes {
            if char.is_whitespace() || char == ',' {
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
        bail!("found EOL while parsing string: `{string}`")
    } else {
        if buf.len() > 0 {
            result.push(buf.to_string());
        }
        Ok(result)
    }
}

pub fn parse_line(line: &String) -> Result<Instruction> {
    if line.trim().is_empty() {
        return Ok(Instruction::nop());
    }

    let mut arguments = split_string(line).context("splitting line")?;

    let name = arguments.remove(0);

    Ok(Instruction {
        name: name.trim_start().into(),
        arguments,
    })
}

#[derive(Debug)]
pub struct Ctx<'a> {
    stack: Vec<Primitive>,
    function: &'a Function,
    return_value: Option<Option<Primitive>>,
    call_stack: &'a mut Stack,
}

impl<'a> Ctx<'a> {
    pub fn new(function: &'a Function, call_stack: &'a mut Stack) -> Self {
        Self {
            stack: vec![],
            function,
            return_value: None,
            call_stack,
        }
    }

    pub(crate) fn return_now(&mut self, var: Option<Primitive>) {
        self.return_value = Some(var);
    }

    pub(crate) fn get_return_value(&self) -> Result<&Option<Primitive>> {
        let Some(ref unwrapped) = self.return_value else {
            bail!("the function hasn't been called.")
        };

        Ok(&unwrapped)
    }

    fn clear_and_set_stack(&mut self, var: Primitive) {
        self.stack.clear();
        self.stack.push(var);
    }

    fn stack_size(&self) -> usize {
        self.stack.len()
    }

    fn push(&mut self, var: Primitive) {
        self.stack.push(var);
    }

    fn pop(&mut self) -> Option<Primitive> {
        self.stack.pop()
    }
}

pub struct Instruction {
    pub name: String,
    arguments: Vec<String>,
}

impl Instruction {
    pub fn nop() -> Self {
        Self {
            name: "nop".into(),
            arguments: vec![],
        }
    }
}
