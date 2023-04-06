use super::context::Ctx;
use super::function::InstructionExitState;
use super::stack::{Stack, VariableMapping};
use super::variables::{bin_op_from, bin_op_result, Primitive};
use anyhow::{bail, Context, Result};
use std::cell::Cell;
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::sync::Arc;

pub type InstructionSignature = fn(&mut Ctx, &Vec<String>) -> Result<()>;

macro_rules! instruction {
    ($($name:ident $body:expr)*) => {
        $(
            pub(crate) fn $name(_ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident(ctx=$ctx:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name($ctx: &mut Ctx, _args: &Vec<String>) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident(args=$args:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name(_ctx: &mut Ctx, $args: &Vec<String>) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident($ctx:ident, $args:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name($ctx: &mut Ctx, $args: &Vec<String>) -> Result<()> {
                $body
            }
        )*
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

#[deny(dead_code)]
mod implementations {
    use super::*;
    use crate::bytecode::function::{PrimitiveFunction, ReturnValue};
    use crate::bytecode::variables::{buckets, Variable};
    use crate::{bool, function, int, vector};
    use std::collections::HashMap;

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

                let stack = ctx.get_call_stack();

                println!("\nFunction: {}", ctx.owner());
                println!("\nOperating Stack: {:?}", ctx.get_local_operating_stack());
                println!("\nStack Trace:\n{}", stack);
                println!("\nThis Frame's Variables:\n\t{}\n", stack.get_frame_variables());
            }
            println!("======= End Context Dump =======");

            Ok(())
        }
    }

    instruction! {
        bin_op(ctx, args) {
            let symbols = args.first().context("Expected an operation [+,-,*,/,%]")?.as_bytes();

            let symbol = symbols[0] as char;

            let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
                bail!("bin_op requires two items on the local operating stack.")
            };

            let (i32_fn, i128_fn, f_fn) = bin_op_from(symbol).context("constructing bin op")?;

            let result = bin_op_result(left, right, i32_fn, i128_fn, f_fn)?;

            ctx.clear_and_set_stack(result);

            Ok(())
        }

        vec_op(ctx, args) {
            let mut arg_iter = args.iter();
            let op_name = arg_iter.next().context("Expected a vector operation")?;
            let bytes = op_name.as_bytes();

            if let (Some(b'['), Some(b']')) = (bytes.first(), bytes.last()) {
                let Some(Primitive::Vector(vector)) = ctx.pop() else {
                    bail!("Cannot perform a vector operation on a non-vector")
                };

                let content = &bytes[1..bytes.len() - 1];

                // this manual byte slice to usize conversion is more performant
                let mut idx: usize = 0;
                let mut max = 10_usize.pow(content.len() as u32 - 1);

                for byte in content {
                    if !matches!(byte, b'0'..=b'9') {
                        bail!("'{}' is not numeric", char::from(*byte))
                    }
                    idx += (byte - b'0') as usize * max;
                    max /= 10;
                }

                let item = vector.get(idx)
                    .with_context(|| format!("index {idx} out of bounds (len {})", vector.len()))?;

                ctx.push(item.clone());
            } else {
                match op_name.as_str() {
                    "reverse" => {
                        let Some(Primitive::Vector(buckets::Vector(vector))) = ctx.get_last_op_item_mut() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        vector.reverse()
                    },
                    "mut" => {
                        let idx = arg_iter.next().context("mutating an array requires an argument")?;
                        let idx = usize::from_str_radix(idx, 10)?;

                        // let len = ctx.stack_size();
                        if ctx.stack_size() != 2 {
                            bail!("mutating an array requires two items in the local operating stack")
                        }

                        let new_item = ctx.pop().context("could not pop first item")?;

                        let Some(Primitive::Vector(buckets::Vector(vector))) = ctx.get_last_op_item_mut() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        vector[idx] = new_item;

                    }
                    not_found => bail!("operation not found: `{not_found}`")
                }
            }

            Ok(())
        }
    }

    instruction! {
        nop Ok(())
    }

    instruction! {
        void(ctx=ctx) {
            ctx.clear_stack();
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

            ctx.signal(InstructionExitState::ReturnValue(ReturnValue(var)));

            Ok(())
        }
    }

    make_type!(make_bool);
    make_type!(make_str);
    make_type!(make_int);
    make_type!(make_float);
    make_type!(make_char);
    make_type!(make_byte);
    make_type!(make_bigint);

    instruction! {
        make_function(ctx, args) {
            let Some(location) = args.first() else {
                bail!("making a function pointer requires a path to find it")
            };

            let len = args.len();
            let callback_state = if len != 1 {
                let mut arguments: HashMap<String, Variable> = HashMap::with_capacity(len - 1); // maybe len
                for var_name in &args[1..] {
                    let Some(var) = ctx.load_variable(var_name) else {
                        bail!("{var_name} is not in scope")
                    };

                    arguments.insert(var_name.clone(), var.clone());
                }

                Some(Arc::new(VariableMapping(arguments)))
            } else {
                None
            };

            ctx.push(function!(PrimitiveFunction::new(location.into(), callback_state)));

            Ok(())
        }

        make_vector(ctx, args) {
            if args.len() > 1 {
                bail!("`make_vector` instruction requires 1 argument (capacity) or none (initializes with contents of local operating stack)")
            }

            let Some(arg) = args.first() else {
                let vec = ctx.get_local_operating_stack().clone().into();
                // ^^ new capacity = old length

                ctx.clear_stack();
                ctx.push(vector!(raw vec));

                return Ok(())
            };

            let capacity = usize::from_str_radix(arg, 10).context("argument must be of type usize")?;

            let vec = vector!(raw Vec::with_capacity(capacity));

            ctx.push(vec);

            Ok(())
        }
    }

    instruction! {
        printn(ctx, args) {
            let Some(arg) = args.first() else {
                bail!("expected 1 parameter (index into local operating stack), or * to print all");
            };

            if arg == "*" {
                let Some(first) = ctx.get_nth_op_item(0) else {
                    return Ok(())
                };

                print!("{first}");
                let operating_stack = ctx.get_local_operating_stack();

                for var in operating_stack.iter().skip(1) {
                    print!(", {var}")
                }

                println!();

                return Ok(());
            }

            let arg = usize::from_str_radix(arg, 10).context("argument must be of type usize")?;

            println!("{}", ctx.get_nth_op_item(arg).context("nothing at index")?);

            Ok(())
        }
    }

    instruction! {
        call(ctx, args) {
            // This never needs to re-allocate, but does need to do O(n) data movement 
            // if the circular buffer doesn’t happen to be at the beginning of the 
            // allocation (https://doc.rust-lang.org/std/collections/vec_deque/struct.VecDeque.html)
            let arguments = ctx.get_local_operating_stack().into();

            let Some(first) = args.first() else {
                let last = ctx.pop();

                let Some(Primitive::Function(f)) = last else {
                    bail!("missing argument, and the last item in the local stack {last:?} is not a function.")
                };

                let f: &PrimitiveFunction = &*f;

                ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                    destination_label: f.location.clone(),
                    callback_state: f.callback_state.clone(),
                    stack: ctx.arced_call_stack().clone(),
                    arguments, 
                }));

                ctx.clear_stack();

                return Ok(())
            };

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination_label: first.clone(),
                callback_state: None,
                stack: ctx.arced_call_stack().clone(),
                arguments,
            }));

            ctx.clear_stack();

            Ok(())
        }

        arg(ctx, args) {
            let Some(first) = args.first() else {
                bail!("expected one argument")
            };

            let n = usize::from_str_radix(first, 10).context("argument must be of type usize")?;

            let Some(nth_arg) = ctx.nth_arg(n) else {
                bail!("#{n} argument does not exist (range 0..{})", ctx.argc())
            };

            ctx.push(nth_arg.clone()); // 4/4/2023: why do we need to clone here? maybe re-work this.

            Ok(())
        }
    }

    instruction! {
        stack_size(ctx=ctx) {
            let size = int!(ctx.frames_count().try_into()?);
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

        load(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let Some(var) = ctx.load_variable(&name) else {
                bail!("load before store (`{name}` not in scope)")
            };

            ctx.push(var.data.clone());

            Ok(())
        }

        load_local(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let Some(var) = ctx.load_local(&name) else {
                bail!("load before store (`{name}` not in this stack frame)")
            };

            ctx.push(var.data.clone());

            Ok(())
        }

        load_callback(ctx, args) {
            let Some(name) = args.first() else {
                bail!("loading a callback variable requires one parameter: (name)")
            };

            let Some(var) = ctx.load_callback_variable(name)? else {
                bail!("this callback does not have `{name}`")
            };

            ctx.push(var.data.clone());

            Ok(())
        }
    }

    instruction! {
        typecmp(ctx=ctx) {
            if ctx.stack_size() != 2 {
                bail!("typecmp requires only 2 items in the local stack")
            }

            let first = ctx.pop().unwrap();
            let second = ctx.pop().unwrap();

            let cmp = first.ty() == second.ty();

            ctx.push(bool!(cmp));

            Ok(())
        }

        strict_equ(ctx=ctx) {
            if ctx.stack_size() != 2 {
                bail!("equ requires only 2 items in the local stack")
            }

            let first = ctx.pop().unwrap();
            let second = ctx.pop().unwrap();

            let result = first == second;

            ctx.push(bool!(result));

            Ok(())
        }

        equ(ctx=ctx) {
            if ctx.stack_size() != 2 {
                bail!("equ requires only 2 items in the local stack")
            }

            let first = ctx.pop().unwrap();
            let second = ctx.pop().unwrap();

            let result = first.equals(&second)?;

            ctx.push(bool!(result));

            Ok(())
        }
    }

    instruction! {
        if_stmt(ctx=ctx) {
            if ctx.stack_size() == 0 {
                bail!("if statements require at least one entry in the local stack")
            }

            let item = ctx.pop().unwrap();
            ctx.clear_stack();

            let Primitive::Bool(b) = item else {
                bail!("if statement can only test booleans")
            };

            ctx.signal(InstructionExitState::NewIf(*b));

            Ok(())
        }

        else_stmt(ctx=ctx) {
            ctx.signal(InstructionExitState::GotoElse);

            Ok(())
        }

        endif_stmt(ctx=ctx) {
            ctx.signal(InstructionExitState::GotoEndif);

            Ok(())
        }
    }
}

pub fn query(name: &String) -> InstructionSignature {
    match name.as_str() {
        "nop" | "#" => implementations::nop,
        "constexpr" => implementations::constexpr,
        "stack_dump" => implementations::stack_dump,
        "pop" => implementations::pop,
        "bin_op" => implementations::bin_op,
        "vec_op" => implementations::vec_op,
        "bool" => implementations::make_bool,
        "string" => implementations::make_str,
        "bigint" => implementations::make_bigint,
        "int" => implementations::make_int,
        "float" => implementations::make_float,
        "char" => implementations::make_char,
        "byte" => implementations::make_byte,
        "make_function" => implementations::make_function,
        "make_vector" => implementations::make_vector,
        "void" => implementations::void,
        "breakpoint" => implementations::breakpoint,
        "ret" => implementations::ret,
        "printn" => implementations::printn,
        "call" => implementations::call,
        "stack_size" => implementations::stack_size,
        "store" => implementations::store,
        "load" => implementations::load,
        "load_local" => implementations::load_local,
        "typecmp" => implementations::typecmp,
        "if" => implementations::if_stmt,
        "else" => implementations::else_stmt,
        "endif" => implementations::endif_stmt,
        "strict_equ" => implementations::strict_equ,
        "equ" => implementations::equ,
        "arg" => implementations::arg,
        "load_callback" => implementations::load_callback,
        _ => unreachable!("unknown bytecode instruction ({name})"),
    }
}

#[inline(always)]
pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) -> Result<()> {
    let instruction_fn = query(&instruction.name);
    instruction_fn(ctx, &instruction.arguments)?;
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
    pub callback_state: Option<Arc<VariableMapping>>,
    pub stack: Arc<Cell<Stack>>,
    pub arguments: Vec<Primitive>,
}

impl Debug for JumpRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "goto {}", self.destination_label)
    }
}

#[derive(Debug)]
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
