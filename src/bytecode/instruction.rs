use std::cell::Cell;
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::sync::Arc;

use anyhow::{bail, Context, Result};

use super::function::Function;
use super::stack::Stack;
use super::variable::{bin_op_from, bin_op_result, Primitive};

pub type InstructionSignature = fn(&mut Ctx, &Vec<String>) -> Result<()>;

macro_rules! instruction {
    ($name:ident $body:expr) => {
        pub(crate) fn $name(_ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
            $body
        }
    };
    ($name:ident(ctx=$ctx:ident) $body:expr) => {
        pub(crate) fn $name($ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
            $body
        }
    };
    ($name:ident(args=$args:ident) $body:expr) => {
        pub(crate) fn $name(_ctx: &mut Ctx, $args: &Vec<String>) -> Result<()> {
            $body
        }
    };
    ($name:ident($ctx:ident, $args:ident) $body:expr) => {
        pub(crate) fn $name($ctx: &mut Ctx, $args: &Vec<String>) -> Result<()> {
            $body
        }
    };
}

macro_rules! make_type {
    ($name:ident) => {
        instruction! {
            $name(ctx, args) {
                if args.len() != 1 {
                    bail!("expected 1 parameter")
                }
    
                let var = Primitive::$name(&args[0])?;
    
                ctx.push(var);
    
                Ok(())
            }
        }
    };
}

mod implementations {
    use std::any::Any;

    use super::*;

    instruction! {
        constexpr(ctx, args) {
            if args.len() != 1 {
                bail!("unexpected 1 parameter")
            }

            let var = Primitive::from(args[0].as_str());

            ctx.push(var);

            Ok(())
        }
    }

    instruction! {
        pop(ctx, args) {
            if args.len() != 0 {
                bail!("unexpected parameter")
            }

            ctx.pop();

            Ok(())
        }
    }

    instruction! {
        stack_dump(ctx, args) {
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

                println!("\nFunction: {}\n", ctx.function);
                println!("Stack Trace:\n{}\n", unsafe { (*ctx.call_stack.as_ptr()).to_string() });
                println!("This Frame's Variables:\n\t{:?}\n", unsafe { (*ctx.call_stack.as_ptr()).get_frame_variables() });
            }
            println!("======= End Context Dump =======");

            Ok(())
        }
    }

    instruction! {
        bin_op(ctx, args) {
            let symbols = match args.first() {
                Some(symbols) => symbols,
                None => bail!("Expected an operation [+,-,*,/,%]"),
            }.as_bytes();

            let symbol = symbols[0] as char;

            let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
                unreachable!()
            };

            let (i32_fn, i128_fn, f_fn) = bin_op_from(symbol).context("constructing bin op")?;

            let result = bin_op_result(left, right, i32_fn, i128_fn, f_fn)?;

            ctx.clear_and_set_stack(result);

            Ok(())
        }
    }

    instruction! {
        nop Ok(())
    }

    instruction! {
        void(ctx=ctx) {
            ctx.stack.clear();
            Ok(())
        }
    }

    instruction! {
        breakpoint(ctx, args) {
            print!("[!!] BREAKPOINT\n[!!] options\n[!!] - continue\n[!!] - dump\n[!!] Enter Option: ");

            let mut buf = String::new();
            stdout().flush()?;
            stdin().read_line(&mut buf)?;

            match buf.trim_end() {
                "continue" => Ok(()),
                "dump" => {
                    stack_dump(ctx, args)
                }
                buf => {
                    println!("[!!]\n[!!] BREAKPOINT\n[!!] '{buf}' is not a valid option.\n[!!]");
                    breakpoint(ctx, args)
                }
            }
        }
    }

    instruction! {
        ret(ctx=ctx) {
            if ctx.stack_size() > 1 {
                bail!("ret can only return a single item");
            }

            let var = ctx.pop();

            ctx.return_now(var);

            Ok(())
        }
    }

    make_type!(make_bool);
    make_type!(make_str);
    make_type!(make_int);
    make_type!(make_float);
    make_type!(make_char);
    make_type!(make_byte);

    instruction! {
        printn(ctx, args) {
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
    }

    instruction! {
        call(ctx, args) {
            let Some(first) = args.first() else {
                bail!("expected one argument");
            };

            ctx.add_frame(first.to_string());

            ctx.request_jump(first.clone(), None, Arc::clone(&ctx.call_stack));

            Ok(())
        }
    }

    instruction! {
        stack_size(ctx=ctx) {
            let size = Primitive::Int(ctx.frames_count().try_into()?);
            ctx.push(size);

            Ok(())
        }
    }

    instruction! {
        store(ctx, args) {
            let Some(name) = args.first() else {
                bail!("store requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("store can only store a single item");
            }

            let arg = ctx.pop().unwrap();

            ctx.register_variable(name.clone(), arg);

            Ok(())
        }
    }
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
        "call" => implementations::call,
        "stack_size" => implementations::stack_size,
        "store" => implementations::store,
        _ => unreachable!("unknown bytecode instruction ({name})"),
    }
}

pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) -> Result<()> {
    let x = query(&instruction.name);
    x(ctx, &instruction.arguments)?;

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

#[derive(Clone)]
pub struct JumpRequest {
    pub destination_label: String,
    pub caller: Option<fn() -> String>,
    pub stack: Arc<Cell<Stack>>,
}

pub struct Ctx<'a> {
    stack: Vec<Primitive>,
    function: &'a Function,
    return_value: Option<Option<Primitive>>,
    call_stack: Arc<Cell<Stack>>,
    requested_jump: Option<JumpRequest>,
}

impl Debug for Ctx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ctx")
    }
}

impl<'a> Ctx<'a> {
    pub fn new(function: &'a Function, call_stack: Arc<Cell<Stack>>) -> Self {
        Self {
            stack: vec![],
            function,
            return_value: None,
            call_stack,
            requested_jump: None,
        }
    }

    pub(crate) fn add_frame(&self, label: String) {
        unsafe { (*self.call_stack.as_ptr()).extend(label) }
    }

    pub(crate) fn frames_count(&self) -> usize {
        unsafe { (*self.call_stack.as_ptr()).size() }
    }

    pub(crate) fn request_jump(
        &mut self,
        destination_label: String,
        caller: Option<fn() -> String>,
        stack: Arc<Cell<Stack>>,
    ) {
        self.requested_jump = Some(JumpRequest {
            destination_label,
            caller,
            stack,
        })
    }

    pub(crate) fn clear_and_get_jump_request(&mut self) -> Option<JumpRequest> {
        let res = self.requested_jump.clone();
        self.requested_jump = None;
        res
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

    pub(crate) fn clear_and_set_stack(&mut self, var: Primitive) {
        self.stack.clear();
        self.stack.push(var);
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }

    pub(crate) fn push(&mut self, var: Primitive) {
        self.stack.push(var);
    }

    pub(crate) fn pop(&mut self) -> Option<Primitive> {
        self.stack.pop()
    }

    pub(crate) fn register_variable(&self, name: String, var: Primitive) {
        unsafe {
            (*self.call_stack.as_ptr()).register_variable(name, var)
        }
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
