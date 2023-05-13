use super::context::Ctx;
use super::function::InstructionExitState;
use super::instruction_constants;
use super::stack::{Stack, VariableMapping};
use super::variables::{ObjectBuilder, Primitive};
use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::sync::Arc;

static mut OBJECT_BUILDER: Lazy<ObjectBuilder> = Lazy::new(|| ObjectBuilder::new());

pub type InstructionSignature = fn(&mut Ctx, &[String]) -> Result<()>;

macro_rules! instruction {
    ($($name:ident $body:expr)*) => {
        $(
            pub(crate) fn $name(_ctx: &mut Ctx, _args: &[String]) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident(ctx=$ctx:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name($ctx: &mut Ctx, _args: &[String]) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident(args=$args:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name(_ctx: &mut Ctx, $args: &[String]) -> Result<()> {
                $body
            }
        )*
    };
    ($($name:ident($ctx:ident, $args:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name($ctx: &mut Ctx, $args: &[String]) -> Result<()> {
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
pub mod implementations {
    use super::*;
    use crate::arc_to_ref;
    use crate::context::SpecialScope;
    use crate::function::{PrimitiveFunction, ReturnValue};
    use crate::stack::VariableFlags;
    use crate::variables::Object;
    use crate::{bool, function, int, object, vector};
    use std::collections::HashMap;
    use std::sync::Arc;

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

                let stack = ctx.get_call_stack_string();

                println!("\nFunction: {}", ctx.owner());
                println!("\nOperating Stack: {:?}", ctx.get_local_operating_stack());
                println!("\nStack Trace:\n{}", stack);
                println!("\nThis Frame's Variables:\n\t{}\n", ctx.get_frame_variables());
            }
            println!("======= End Context Dump =======");

            Ok(())
        }
    }

    instruction! {
        bin_op(ctx, args) {
            use Primitive::*;
            let symbols = args.first().context("Expected an operation [+,-,*,/,%,>,>=,<,<=]")?;

            let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
                bail!("bin_op requires two items on the local operating stack (found {:?})", ctx.get_local_operating_stack())
            };

            let result = match (symbols.as_str(), &left, &right) {
                ("+", ..) => left + right,
                ("-", ..) => left - right,
                ("*", ..) => left * right,
                ("/", ..) => left / right,
                ("%", ..) => left % right,
                (">", ..) => Ok(bool!(left > right)),
                ("<", ..) => Ok(bool!(left < right)),
                (">=", ..) => Ok(bool!(left >= right)),
                ("<=", ..) => Ok(bool!(left <= right)),
                ("=", ..) => Ok(bool!(left.equals(&right)?)),
                ("and", Bool(x), Bool(y)) => Ok(bool!(*x && *y)),
                ("or", Bool(x), Bool(y)) => Ok(bool!(*x || *y)),
                ("xor", Bool(x), Bool(y)) => Ok(bool!(*x ^ *y)),
                _ => bail!("unknown operation: {symbols}")
            }.context("invalid binary operation")?;

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
                        let Some(Primitive::Vector(vector)) = ctx.get_last_op_item_mut() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        let reference = Arc::get_mut(vector).context("could not get reference to vector")?;
                        reference.reverse();
                    },
                    "mut" => {
                        let idx = arg_iter.next().context("mutating an array requires an argument")?;
                        let idx = usize::from_str_radix(idx, 10)?;

                        if ctx.stack_size() != 2 {
                            bail!("mutating an array requires two items in the local operating stack")
                        }

                        let new_item = ctx.pop().context("could not pop first item")?;

                        let Some(Primitive::Vector(vector)) = ctx.get_last_op_item_mut() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        Arc::get_mut(vector).context("could not get reference to vector")?[idx] = new_item;

                    }
                    not_found => bail!("operation not found: `{not_found}`")
                }
            }

            Ok(())
        }
    }

    instruction! {
        nop bail!("nop instructions are deprecated")
    }

    instruction! {
        len(ctx=ctx) {
            let Some(top) = ctx.pop() else {
                bail!("len requires an item on the local stack")
            };

            let result = match top {
                Primitive::Vector(v) => int!(v.len().try_into()?),
                Primitive::Str(s) => int!(s.len().try_into()?),
                _ => bail!("cannot get the raw length of a non-string/vector"),
            };

            ctx.push(result);

            Ok(())
        }

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

            let ret = if let Some(primitive) = var {
                ReturnValue::Value(primitive)
            } else {
                ReturnValue::NoValue
            };

            ctx.signal(InstructionExitState::ReturnValue(ret));

            Ok(())
        }
    }

    make_type!(make_bool);
    make_type!(make_int);
    make_type!(make_float);
    make_type!(make_byte);
    make_type!(make_bigint);

    instruction! {
        make_str(ctx, args) {
            let raw_str = match args.len() {
                0 => "",
                1 => &args[0],
                _ => bail!("make_str requires 0 arguments (empty string) or one argument (the string)")
            };

            let var = crate::string!(raw raw_str);

            ctx.push(var);

            Ok(())
        }

        make_function(ctx, args) {
            let Some(location) = args.first() else {
                bail!("making a function pointer requires a path to find it")
            };

            let len = args.len();
            let callback_state = if len != 1 {
                let mut arguments = HashMap::with_capacity(len - 1); // maybe len
                for var_name in &args[1..] {
                    let Some(var) = ctx.load_variable(var_name) else {
                        bail!("{var_name} is not in scope")
                    };

                    arguments.insert(var_name.clone(), (var.0.clone(), VariableFlags::none()));
                }

                Some(Arc::new(arguments.into()))
            } else {
                None
            };

            ctx.push(function!(PrimitiveFunction::new(location.into(), callback_state)));

            Ok(())
        }

        make_object(ctx, args) {
            if args.len() != 0 {
                bail!("`make_object` does not require arguments")
            }

            let object_variables = Arc::new(ctx.get_frame_variables().clone());

            let function = ctx.owner();
            let name = Arc::new(function.name.clone());

            let obj = unsafe {
                if !OBJECT_BUILDER.has_class_been_registered(&name) {
                    let location = &function.location;
                    let object_path = format!("{}#{name}$", location.path);

                    let object_functions = arc_to_ref(location).get_object_functions(&object_path)?;

                    let mut mapping: HashSet<String> = HashSet::new();

                    for func in object_functions {
                        mapping.insert(func.get_qualified_name());
                    }

                    OBJECT_BUILDER.register_class(Arc::clone(&name), mapping);
                }

                OBJECT_BUILDER.name(Arc::clone(&name)).object_variables(object_variables).build()
            };

            ctx.push(object!(Arc::new(obj)));

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
                    println!();
                    return Ok(())
                };

                print!("{first}");
                let operating_stack = ctx.get_local_operating_stack();

                for var in operating_stack.iter().skip(1) {
                    print!(", {var}")
                }

                stdout().flush()?;

                println!();

                return Ok(());
            }

            let arg = usize::from_str_radix(arg, 10).context("argument must be of type usize")?;

            println!("{}", ctx.get_nth_op_item(arg).context("nothing at index")?);

            Ok(())
        }
    }

    instruction! {
        call_object(ctx, args) {
            let path = args.last().context("missing method name argument")?;

            let Some(first) = ctx.get_nth_op_item(0) else {
                bail!("there is no item in the local stack")
            };

            let Primitive::Object(o) = first else {
                bail!("last item in the local stack {first:?} is not an object.")
            };

            let callback_state = Some(o.object_variables.clone());

            let arguments = ctx.get_local_operating_stack();
            ctx.clear_stack();

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Standard(path.clone()),
                callback_state,
                stack: ctx.arced_call_stack().clone(),
                arguments,
            }));

            Ok(())
        }

        mutate(ctx, args) {
            let var_name = args.first().context("object mutation requires a name argument")?;

            if ctx.stack_size() != 2 {
                bail!("mutating an object requires two items in the local operating stack (obj, data)")
            }

            let new_item: Primitive = ctx.pop().context("could not pop first item")?.into();

            let Some(Primitive::Object(o)) = ctx.get_last_op_item_mut() else {
                bail!("Cannot perform an object mutation on a non-object")
            };

            unsafe {
                // this bypass of Arc protections is messy and should be refactored.
                let var = (*(Arc::as_ptr(&o) as *mut Object)).has_variable_mut(var_name).context("variable does not exist on object")?;

                if var.0.ty() != new_item.ty() {
                    bail!("mismatched types in assignment ({:?} & {:?})", var.0.ty(), new_item.ty())
                }

                var.0 = new_item.into();
            }

            Ok(())
        }

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

                ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                    destination: JumpRequestDestination::Standard(f.location.clone()),
                    callback_state: f.callback_state.clone(),
                    stack: ctx.arced_call_stack().clone(),
                    arguments,
                }));

                ctx.clear_stack();

                return Ok(())
            };

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Standard(first.clone()),
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

        update(ctx, args) {
            let Some(name) = args.first() else {
                bail!("store requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("store can only store a single item");
            }

            let arg = ctx.pop().unwrap();

            ctx.update_variable(name.clone(), arg)?;

            Ok(())
        }

        store_object(ctx, args) {
            let Some(name) = args.first() else {
                bail!("store_object requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("store can only store a single item");
            }

            let arg = ctx.pop().unwrap();

            ctx.update_callback_variable(name.to_string(), arg.into())?;

            Ok(())
        }

        load(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let Some(var) = ctx.load_variable(&name) else {
                bail!("load before store (`{name}` not in scope)")
            };

            ctx.push(var.0.clone());

            Ok(())
        }

        load_local(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let Some(var) = ctx.load_local(&name) else {
                bail!("load before store (`{name}` not in this stack frame)")
            };

            ctx.push(var.0.clone());

            Ok(())
        }

        load_callback(ctx, args) {
            let Some(name) = args.first() else {
                bail!("loading a callback variable requires one parameter: (name)")
            };

            let var = ctx.load_callback_variable(name)?;

            ctx.push(var.clone());

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
        if_stmt(ctx, args) {
            if ctx.stack_size() == 0 {
                bail!("if statements require at least one entry in the local stack")
            }

            let item = ctx.pop().unwrap();
            ctx.clear_stack();

            let Primitive::Bool(b) = item else {
                bail!("if statement can only test booleans")
            };

            let Some(offset) = args.first() else {
                bail!("if statements require an argument to instruct where to jump if falsey")
            };

            if !b {
                dbg!("falsey; jumping...");
                ctx.signal(InstructionExitState::GotoPushScope(usize::from_str_radix(offset, 10)?, SpecialScope::If));
            }


            Ok(())
        }

        jmp(ctx, args) {
            let Some(offset) = args.first() else {
                bail!("jmp statements require an argument to instruct where to jump")
            };

            ctx.signal(InstructionExitState::Goto(usize::from_str_radix(offset, 10)?));

            Ok(())
        }

        step_iter(_ctx, _args) {
            #[allow(unused)]
            let (ctx, args) = (_ctx, _args);

            unimplemented!();

            let Some(Ok(end_idx)) = args.get(0).map(|s| i32::from_str_radix(s, 10)) else {
                bail!("step_iter statements require an argument to instruct where to jump after completion")
            };

            let Some(binding_name) = args.get(1) else {
                bail!("step_iter statements require a binding name")
            };

            let Some(Ok(start_inclusive)) = args.get(2).map(|s| i32::from_str_radix(s, 10)) else {
                bail!("step_iter statements require a start position (inclusive)")
            };

            let Some(Ok(stop_exclusive)) = args.get(3).map(|s| i32::from_str_radix(s, 10)) else {
                bail!("step_iter statements require a stop position (exclusive)")
            };

            let step = args.get(4).map(|s| i32::from_str_radix(s, 10)).unwrap_or(Ok(1))?;

            match ctx.load_local(binding_name) {
                Some((_val, flags)) if flags.is_loop_variable() => {
                    //
                }
                None => {
                    ctx.push(int!(start_inclusive))
                }
                _ => {
                    bail!("step_iter could not find the variable associated with this loop. Has it been dropped?")
                }
            }

            // let value = if let Some(val) = ctx.load_variable(binding_name) {
            //     val
            // }

            // ctx.register_variable(binding_name, )
            dbg!(end_idx, binding_name, start_inclusive, stop_exclusive, step);

            Ok(())
        }

        done(ctx, _args) {
            ctx.signal(InstructionExitState::PopScope);

            Ok(())
        }

        else_stmt(ctx, _args) {
            ctx.signal(InstructionExitState::PushScope(SpecialScope::Else));

            Ok(())
        }
    }

    instruction! {
        call_lib(ctx, args) {
            let (Some(lib_name), Some(func_name)) = (args.get(0), args.get(1)) else {
                bail!("expected syntax: call_lib path/to/lib.dll function_name")
            };

            let arguments = ctx.get_local_operating_stack().into();

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Library {
                    lib_name: lib_name.clone(),
                    func_name: func_name.clone(),
                },
                // destination_label: JumpRequestDestination::Standard(first.clone()),
                callback_state: None,
                stack: ctx.arced_call_stack().clone(),
                arguments,
            }));

            ctx.clear_stack();
            Ok(())
        }
    }
}

pub fn query(byte: u8) -> InstructionSignature {
    instruction_constants::FUNCTION_POINTER_LOOKUP[byte as usize]
}

#[inline(always)]
pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) -> Result<()> {
    let instruction_fn = query(instruction.id);
    instruction_fn(ctx, &instruction.arguments)?;
    Ok(())
}

pub fn split_string(string: Cow<str>) -> Result<Box<[String]>> {
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
                if escaping {
                    buf.push(char);

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
        Ok(result.into_boxed_slice())
    }
}

#[derive(Clone, Debug)]
pub enum JumpRequestDestination {
    Standard(String),
    Library { lib_name: String, func_name: String },
}

#[derive(Clone, Debug)]
pub struct JumpRequest {
    pub destination: JumpRequestDestination,
    pub callback_state: Option<Arc<VariableMapping>>,
    pub stack: Arc<Stack>,
    pub arguments: Vec<Primitive>,
}

#[derive(Debug)]
pub struct Instruction {
    pub id: u8,
    arguments: Box<[String]>,
}

impl Instruction {
    pub fn new(id: u8, arguments: Box<[String]>) -> Self {
        Instruction { id, arguments }
    }
}
