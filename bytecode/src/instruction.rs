//! All of the implementations for each bytecode instruction are found here.

use super::context::Ctx;
use super::function::InstructionExitState;
use super::instruction_constants;
use super::stack::{Stack, VariableMapping};
use super::variables::{ObjectBuilder, Primitive};
use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;

/// This variable allows instructions to register objects on the fly.
static mut OBJECT_BUILDER: Lazy<ObjectBuilder> = Lazy::new(ObjectBuilder::new);

/// Used for type declarations in lookup tables.
pub type InstructionSignature = fn(&mut Ctx, &[String]) -> Result<()>;

/// A macro to help generate the boilerplate around instruction implementations.
///
/// ## Note about the return value:
///
/// The downside of macros is that the return type of each function is hidden.
/// Each instruction is expected to return [`anyhow::Result<()>`], and if it
/// has a special exit state, to pass it through the context ([`Ctx`]).
///
/// This macro takes the shape...
///
/// ```ignore
/// instruction! {
///     name(...) {
///         // impl
///     }
///
///     ...
/// }
/// ```
///
/// ...and has multiple valid implementations.
///
/// # Examples
/// * No args:
/// ```ignore
/// instruction! {
///     nop() Ok(InstructionExitState::NoExit)
/// }
/// ```
/// * Only ctx
/// ```ignore
/// instruction! {
///     do_something_without_args(ctx=my_name_for_context) {
///         // impl
///
///         Ok(())
///     }
/// }
/// ```
/// * Ctx and Args
/// ```ignore
/// instruction! {
///     both_args_and_ctx(ctx=my_name_for_context, args=my_name_for_args) {
///         // impl
///
///         Ok(())
///     }
/// }
/// ```
///
/// # Errors
/// Each instruction can fail spectacularly in its own way. Thus, as an instruction
/// author, one must ensure that error messages capture the essense of how the program
/// blew up. If an instruction errors, it is considered a fatal error by the interpreter
/// and the **entire** program will shut down.
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
    ($($name:ident($ctx:ident, $args:ident) $body:expr)*) => {
        $(
            pub(crate) fn $name($ctx: &mut Ctx, $args: &[String]) -> Result<()> {
                $body
            }
        )*
    };
}

/// Shorthand for identical instructions:
/// * `make_bool`
/// * `make_int`
/// * `make_float`
/// * `make_byte`
/// * `make_bigint`
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

/// This submodule contains the implementations for
/// each instruction, and nothing more.
#[deny(dead_code)]
pub mod implementations {
    use super::*;
    use crate::context::SpecialScope;
    use crate::function::{PrimitiveFunction, ReturnValue};
    use crate::stack::flag_constants::READ_ONLY;
    use crate::stack::VariableFlags;
    use crate::variables::HeapPrimitive;
    use crate::{bool, function, int, object, optional, vector};
    use std::collections::HashMap;
    use std::ops::Deref;
    use std::rc::Rc;

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
            if !args.is_empty() {
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
                println!("\nStack Trace:\n{stack}");
                println!("\nThis Frame's Variables:\n\t{}\n", ctx.get_frame_variables()?);
            }
            println!("======= End Context Dump =======");

            Ok(())
        }
    }

    instruction! {
        neg(ctx, _args) {
            let Some(val) = ctx.get_last_op_item_mut() else {
                bail!("neg requires one item on the local operating stack")
            };

            val.negate()?;

            Ok(())
        }

        not(ctx, _args) {
            let Some(val) = ctx.get_last_op_item_mut() else {
                bail!("not requires one item on the local operating stack")
            };

            let Primitive::Bool(val) = val else {
                bail!("not can only negate booleans")
            };

            *val = !*val;

            Ok(())
        }

        bin_op(ctx, args) {
            use Primitive::*;
            let symbols = args.first().context("Expected an operation [+,-,*,/,%,>,>=,<,<=]")?;

            let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
                bail!("bin_op requires two items on the local operating stack (found {:?})", ctx.get_local_operating_stack())
            };

            log::debug!("{left} {symbols} {right}");

            let left = unsafe { left.move_out_of_heap_primitive() };
            let right = unsafe { right.move_out_of_heap_primitive() };

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
                ("&&", Bool(x), Bool(y)) => Ok(bool!(*x && *y)),
                ("||", Bool(x), Bool(y)) => Ok(bool!(*x || *y)),
                ("^", Bool(x), Bool(y)) => Ok(bool!(*x ^ *y)),
                _ => bail!("unknown operation: {symbols}")
            }.context("invalid binary operation")?;

            ctx.clear_and_set_stack(result);

            Ok(())
        }

        bin_op_assign(ctx, args) {

            let name = args.first().context("you must supply a name")?;
            let op = args.get(1).context("Expected an operation [+=,-=,*=,/=,%=]")?;

            let bundle = ctx.load_variable(name).with_context(|| format!("{name} has not been mapped"))?;
            let value: &mut Primitive = ctx.get_last_op_item_mut().context("there must be a value at the top of the stack for a `bin_op_assign`")?;

            let result = {
                let view = bundle.borrow();
                let view = view.deref();

                match op.as_str() {
                    "+=" => {
                        let no_mut: &Primitive = value;
                        (&view.0 + no_mut)?
                    }
                    "-=" => {
                        let no_mut: &Primitive = value;
                        (&view.0 - no_mut)?
                    }
                    "*=" => {
                        let no_mut: &Primitive = value;
                        (&view.0 * no_mut)?
                    }
                    "/=" => {
                        let no_mut: &Primitive = value;
                        (&view.0 / no_mut)?
                    }
                    "%=" => {
                        let no_mut: &Primitive = value;
                        (&view.0 % no_mut)?
                    }
                    _ => bail!("unknown assignment operation: {op}")
                }
            };

            let mut view = bundle.borrow_mut();
            view.0 = result.clone(); 
            *value = result;

            Ok(())
        }

        ptr_mut(ctx, _args) {
            if ctx.stack_size() < 2 {
                bail!("mutating a ptr requires [ptr, value] (found: {:?})", ctx.get_local_operating_stack());
            }

            let new_val = ctx.pop().unwrap();
            let maybe_vec = ctx.pop().unwrap();

            let Primitive::HeapPrimitive(vec_ptr) = maybe_vec else {
                bail!("expected a mutable heap primitive, found {maybe_vec}");
            };

            unsafe {
                vec_ptr.set(new_val);
            }

            Ok(())
        }

        vec_op(ctx, args) {
            let mut arg_iter = args.iter();
            let op_name = arg_iter.next().context("Expected a vector operation")?;
            let bytes = op_name.as_bytes();

            if let [b'+', ..] = bytes {
                if ctx.stack_size() != 1 {
                    bail!("vec_op +push operations require only a single item on the operating stack")
                }

                let new_val = ctx.pop().unwrap();

                let primitive_with_flags: Rc<RefCell<(Primitive, VariableFlags)>> = ctx.load_local(&op_name[1..]).context("vector not found for pushing")?;

                let primitive_with_flags = primitive_with_flags.borrow();
                // let primitive_with_flags: &mut (Primitive, VariableFlags) = Rc::make_mut(&mut primitive_with_flags);

                let Primitive::Vector(ref vector) = primitive_with_flags.0 else {
                    bail!("not a vector, trying to push")
                };


                // let vector: &mut Vec<Primitive> = rc_to_ref(vector);

                vector.borrow_mut().push(new_val);
            } else if let [b'[', index @ .., b']'] = bytes {
                let Some(indexable) = ctx.pop() else {
                    bail!("the stack is empty");
                };

                // this manual byte slice to usize conversion is more performant
                let idx: usize = 'index_gen: {
                    let mut idx = 0;
                    let mut max = 10_usize.pow(index.len() as u32 - 1);

                    for byte in index {
                        if !byte.is_ascii_digit() {
                            let index_as_str = std::str::from_utf8(index)?;
                            let variable = ctx.load_local(index_as_str).with_context(|| format!("'{index_as_str}' is not a literal number nor a name that has been mapped locally"))?;
                            let variable = variable.borrow();
                            let primitive_part: &Primitive = &variable.0;

                            let as_index: usize = primitive_part.try_into_numeric_index()?;

                            break 'index_gen as_index;
                        }
                        idx += (byte - b'0') as usize * max;
                        max /= 10;
                    }

                    idx
                };

                match indexable {
                    Primitive::Vector(vector) => {
                        let mut vector = vector.borrow_mut();
                        let vec_len = vector.len();

                        let ptr = vector.get_mut(idx).with_context(|| format!("index {idx} out of bounds (len {vec_len})"))? as *mut _;

                        let heap_primitive = HeapPrimitive::new_array_view(ptr);

                        ctx.push(Primitive::HeapPrimitive(heap_primitive));

                    }
                    Primitive::Str(string) => {
                        let mut str_chars = string.chars();
                        ctx.push(Primitive::Str(str_chars.nth(idx).with_context(|| format!("index {idx} out of bounds (len {} chars, {} bytes)", string.chars().count(), string.len()))?.to_string()))
                    }
                    _ => bail!("Cannot perform a vector operation on a non-vector"),
                }
            } else {
                match op_name.as_str() {
                    "reverse" => {
                        let Some(Primitive::Vector(vector)) = ctx.get_last_op_item() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        let mut vector = vector.borrow_mut();

                        vector.reverse();

                        // let reference = Rc::get_mut(vector).context("could not get reference to vector")?;
                        // reference.reverse();
                    },
                    "mut" => {
                        let idx = arg_iter.next().context("mutating an array requires an argument")?;
                        let idx = idx.parse::<usize>()?;

                        if ctx.stack_size() != 2 {
                            bail!("mutating an array requires two items in the local operating stack")
                        }

                        let new_item = ctx.pop().context("could not pop first item")?;

                        let Some(Primitive::Vector(vector)) = ctx.get_last_op_item() else {
                            bail!("Cannot perform a vector operation on a non-vector")
                        };

                        let mut vector = vector.borrow_mut();

                        vector[idx] = new_item;

                        // Rc::get_mut(vector).context("could not get reference to vector")?[idx] = new_item;

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
                Primitive::Vector(v) => int!(v.borrow().len().try_into()?),
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

                    arguments.insert(var_name.clone(), var.clone());
                }

                Some(Rc::new(arguments.into()))
            } else {
                None
            };

            let function_ptr = PrimitiveFunction::new(location.into(), callback_state);

            ctx.push(function!(function_ptr));

            Ok(())
        }

        make_object(ctx, args) {
            if !args.is_empty() {
                bail!("`make_object` does not require arguments")
            }


            let object_variables = {
                let frame_variables = ctx.get_frame_variables()?;
                Rc::new(VariableMapping::clone(&frame_variables))
            };

            let function = ctx.owner();
            let name = Rc::new(function.name().clone());

            let obj = unsafe {
                if !OBJECT_BUILDER.has_class_been_registered(&name) {
                    let location = &function.location();
                    let object_path = format!("{}#{name}$", location.upgrade().unwrap().path());

                    // todo!("pointer hacking ahead...");

                    let location = location.upgrade().unwrap();

                    let mut mapping: HashSet<String> = HashSet::new();

                    let functions_ref = location.get_functions_ref().context("no functions")?;

                    let iter = functions_ref.map.iter().filter_map(move |(key, val)| {
                        if key.starts_with(&object_path) {
                            Some(val)
                        } else {
                            None
                        }
                    });

                    for func in iter {
                        mapping.insert(func.get_qualified_name());
                    }

                    OBJECT_BUILDER.register_class(Rc::clone(&name), mapping);
                }

                OBJECT_BUILDER.name(Rc::clone(&name)).object_variables(object_variables).build()
            };

            ctx.push(object!(Rc::new(RefCell::new(obj))));

            Ok(())
        }

        make_vector(ctx, args) {
            if args.len() > 1 {
                bail!("`make_vector` instruction requires 1 argument (capacity) or none (initializes with contents of local operating stack)")
            }

            let Some(arg) = args.first() else {
                let vec = ctx.get_local_operating_stack();
                // ^^ new capacity = old length

                ctx.clear_stack();
                ctx.push(vector!(raw vec));

                return Ok(())
            };

            let capacity = arg.parse::<usize>().context("argument must be of type usize")?;

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

                log::warn!("The `printn` instruction should not be used. Favor the standard library instead.");

                print!("{first}");
                let operating_stack = ctx.get_local_operating_stack();

                for var in operating_stack.iter().skip(1) {
                    print!(", {var}")
                }

                #[cfg(feature = "debug")]
                stdout().flush()?;

                println!();

                return Ok(());
            }

            let arg = arg.parse::<usize>().context("argument must be of type usize")?;

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

            let callback_state = Some(o.borrow().object_variables.clone());

            let arguments = ctx.get_local_operating_stack();
            ctx.clear_stack();

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Standard(path.clone()),
                callback_state,
                stack: ctx.rced_call_stack(),
                arguments,
            }));

            Ok(())
        }

        mutate(ctx, args) {
            let var_name = args.first().context("object mutation requires a name argument")?;

            if ctx.stack_size() != 2 {
                bail!("mutating an object requires two items in the local operating stack (obj, data)")
            }

            let new_item: Primitive = ctx.pop().context("could not pop first item")?;

            let Some(Primitive::Object(o)) = ctx.get_last_op_item() else {
                bail!("Cannot perform an object mutation on a non-object")
            };

            // this bypass of Rc protections is messy and should be refactored.
            let obj_view = o.borrow();
            let has_variable = obj_view.has_variable(var_name).context("variable does not exist on object")?;
            let mut var = has_variable.borrow_mut();

            if var.0.ty() != new_item.ty() {
                bail!("mismatched types in assignment ({:?} & {:?})", var.0.ty(), new_item.ty())
            }

            // let () = rc_to_ref(&var.0);
            var.0 = new_item;
            // let x = &mut rc_to_ref(&var).0;
            // *x = new_item;
            // var.0 = new_item;

            Ok(())
        }

        call(ctx, args) {
            // This never needs to re-allocate, but does need to do O(n) data movement
            // if the circular buffer doesn’t happen to be at the beginning of the
            // allocation (https://doc.rust-lang.org/std/collections/vec_deque/struct.VecDeque.html)

            let Some(first) = args.first() else {
                let last = ctx.pop();

                let Some(Primitive::Function(f)) = last else {
                    bail!("missing argument, and the last item in the local stack ({last:?}, S:{:#?}) is not a function.", ctx.get_local_operating_stack())
                };

                let destination = JumpRequestDestination::Standard(f.location().clone());

                let callback_state = f.callback_state().clone();

                ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                    destination,
                    callback_state,
                    stack: ctx.rced_call_stack(),
                    arguments: ctx.get_local_operating_stack(),
                }));

                ctx.clear_stack();

                return Ok(())
            };

            let arguments = ctx.get_local_operating_stack();

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Standard(first.clone()),
                callback_state: None,
                stack: ctx.rced_call_stack(),
                arguments,
            }));

            ctx.clear_stack();

            Ok(())
        }

        call_self(ctx, _args) {
            let arguments = ctx.get_local_operating_stack();

            let callback_state = ctx.get_callback_variables();

            let stack = ctx.rced_call_stack();

            let name = {
                let stack_view = stack.borrow();
                stack_view.get_executing_function_label().context("not run in a function")?.to_owned()
            };

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Standard(name),
                callback_state,
                stack,
                arguments
            }));

            ctx.clear_stack();

            Ok(())
        }

        arg(ctx, args) {
            let Some(first) = args.first() else {
                bail!("expected one argument")
            };

            let n = first.parse::<usize>().context("argument must be of type usize")?;

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
        reserve_primitive(ctx=ctx) {
            ctx.push(optional!(empty));

            Ok(())
        }
    }

    instruction! {
        unwrap_into(ctx, args) {
            let Some(name) = args.first() else {
                bail!("`unwrap_into` requires a name");
            };

            let Some(primitive) = ctx.pop() else {
                bail!("`unwrap_into` requires a primitive at the top of the local operating stack");
            };

            let status = match primitive {
                Primitive::Optional(Some(unwrapped)) => {
                    let var = unwrapped.as_ref().to_owned();

                    let var = unsafe { var.move_out_of_heap_primitive() };

                    ctx.register_variable_local(name.to_owned(), var)?;
                    true
                }
                primitive @ Primitive::Optional(None) => {
                    let primitive = unsafe { primitive.move_out_of_heap_primitive() };

                    ctx.register_variable_local(name.to_owned(), primitive)?;
                    false
                },
                other_primitive => {
                    let other_primitive = unsafe { other_primitive.move_out_of_heap_primitive() };

                    ctx.register_variable_local(name.to_owned(), other_primitive)?;
                    true
                }
            };

            ctx.push(bool!(status));

            Ok(())
        }

        unwrap(ctx, args) {
            let Some(primitive) = ctx.get_last_op_item_mut() else {
                bail!("`unwrap` requires a primitive at the top of the local operating stack");
            };

            if let Primitive::Optional(optional) = primitive {
                if let Some(new_primitive) = optional {
                    *primitive = *new_primitive.clone();
                } else {
                    let span = args.get(0).map(String::as_str);
                    bail!("LOGIC ERROR IN CODE >> {} >> unwrap of `nil`", span.unwrap_or("<no details>"));
                }
            }

            Ok(())
        }

        jmp_not_nil(ctx, args) {
            let Some(primitive) = ctx.get_last_op_item_mut() else {
                bail!("`jmp_not_nil` requires a primitive at the top of the local operating stack");
            };

            let Some(lines_to_jump) = args.get(0) else {
                bail!("`jmp_not_nil` requires lines_to_jump");
            };

            let lines_to_jump = lines_to_jump.parse::<isize>().context("jmp_not_nil needs lines_to_jump: isize")?;

            if let Primitive::Optional(None) = primitive {
                ctx.pop();
                return Ok(())
            }

            ctx.signal(InstructionExitState::Goto(lines_to_jump));

            Ok(())
        }
    }

    instruction! {
        store(ctx, args) {
            let Some(name) = args.first() else {
                bail!("`store` requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("`store` can only store a single item (found: {:?})", ctx.get_local_operating_stack());
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };

            ctx.register_variable(Cow::Owned(name.to_owned()), arg)?;

            Ok(())
        }

        export_name(ctx, args) {
            let Some(name) = args.first() else {
                bail!("`export_name` requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("`export_name` can only store a single item (found: {:?})", ctx.get_local_operating_stack());
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };

            let pair = Rc::new(RefCell::new((arg, VariableFlags::new_public())));

            ctx.register_export(name.to_owned(), pair)?;

            Ok(())
        }

        load_self_export(ctx, args) {
            let Some(src) = args.first() else {
                bail!("`load_self_export` requires a source name")
            };

            let primitive = ctx
                .load_self_export(src)
                .with_context(|| format!("`{src}` has not been exported from the executing module."))?;

            let primitive = primitive.borrow().0.clone();

            ctx.push(primitive);
            // ctx.ref_variable(Cow::Owned(s.to_owned()), primitive);

            Ok(())
        }

        export_special(ctx, args) {
            let Some(name) = args.first() else {
                bail!("`export_special` requires a name")
            };

            let export_name = args.get(1).unwrap_or(name);

            if ctx.stack_size() != 1 {
                bail!("`export_special` can only store a single item (found: {:?})", ctx.get_local_operating_stack());
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };

            let pair = Rc::new(RefCell::new((arg, VariableFlags(READ_ONLY))));

            ctx.register_export(export_name.to_owned(), Rc::clone(&pair))?;
            ctx.ref_variable(Cow::Owned(name.to_owned()), pair);

            Ok(())
        }

        store_fast(ctx, args) {
            let Some(name) = args.first() else {
                bail!("store_fast requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("store_fast can only store a single item");
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };

            ctx.register_variable_local(name.clone(), arg)?;

            Ok(())
        }

        store_object(ctx, args) {
            let Some(name) = args.first() else {
                bail!("store_object requires a name")
            };

            if ctx.stack_size() != 1 {
                bail!("store_object can only store a single item");
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };

            ctx.update_callback_variable(name, arg)?;

            Ok(())
        }

        store_skip(ctx, args) {
            let Some([name, predicate, lines_to_jump]) = args.get(0..=2) else {
                bail!("store_skip: name:str predicate:u8(1/0) lines_to_jump:isize")
            };

            let predicate = predicate.parse::<u8>().context("store_skip needs predicate: u8")?;
            let lines_to_jump = lines_to_jump.parse::<isize>().context("store_skip needs lines_to_jump: isize")?;

            if lines_to_jump.is_negative() {
                bail!("store_skip can only skip forwards");
            }

            if ctx.stack_size() != 1 {
                bail!("store_skip can only store a single item");
            }

            let arg = ctx.get_last_op_item().unwrap();

            let Primitive::Bool(val) = arg else {
                bail!("store_skip can only operate on bool (found {arg})");
            };

            if predicate == 1 {
                // skip if true
                if *val {
                    ctx.signal(InstructionExitState::Goto(lines_to_jump));
                    return Ok(())
                }

            } else {
                // skip if false
                if !val {
                    ctx.signal(InstructionExitState::Goto(lines_to_jump));
                    return Ok(())
                }
            }

            let arg = ctx.pop().unwrap();

            let arg = unsafe { arg.move_out_of_heap_primitive() };
            ctx.register_variable_local(name.clone(), arg)?;

            Ok(())
        }

        fast_rev2(ctx, _args) {
            if ctx.stack_size() != 2 {
                bail!("fast_rev2 requires a stack size of 2");
            }

            let Some([first, second]) = ctx.get_many_op_items_mut(0..2) else {
                bail!("could not get op items");
            };

            let first_cloned = first.clone();
            *first = second.clone();
            *second = first_cloned;

            Ok(())
        }

        load(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let Some(var) = ctx.load_variable(name) else {
                bail!("load before store (`{name}` not in scope)")
            };

            ctx.push(var.borrow().0.clone());

            Ok(())
        }

        load_fast(ctx, args) {
            let Some(name) = args.first() else {
                bail!("load requires a name")
            };

            let var = ctx.load_local(name).with_context(|| format!("load before store (`{name}` not in this stack frame)\nframe `{:?}`'s variables:\n{:?}", ctx.rced_call_stack().borrow().get_frame_label(), ctx.get_frame_variables()))?;

            ctx.push(var.borrow().0.clone());

            Ok(())
        }

        load_callback(ctx, args) {
            let Some(name) = args.first() else {
                bail!("loading a callback variable requires one parameter: (name)")
            };

            let var = ctx.load_callback_variable(name)?;

            ctx.push(var.borrow().0.clone());

            Ok(())
        }

        delete_name_scoped(ctx, args) {
            if args.is_empty() {
                bail!("delete_name_scoped requires names to delete")
            };

            for name in args {
                ctx.delete_variable_local(name)?;
            }

            Ok(())
        }

        delete_name_reference_scoped(ctx, args) {
            if args.len() != 1 {
                bail!("delete_name_reference_scoped can only delete to retrieve one name");
            }

            let name = args.first().unwrap();

            let deleted: Rc<RefCell<(Primitive, VariableFlags)>> = ctx.delete_variable_local(name)?;
            let primitive: Primitive = deleted.borrow().0.clone();

            ctx.push(primitive);

            Ok(())
        }

        lookup(ctx, args) {
            if ctx.stack_size() != 1 {
                bail!("`lookup` requires a single item on the stack (found {:?})", ctx.get_local_operating_stack());
            }

            let Some(name) = args.get(0) else {
                bail!("`lookup` requires a name argument");
            };

            let primitive = ctx.pop().unwrap();

            let result: Rc<RefCell<(Primitive, VariableFlags)>> = primitive.lookup(name).with_context(|| format!("`{name}` does not exist on `{primitive:?}`"))?;

            let heap_primitive = Primitive::HeapPrimitive(HeapPrimitive::new_lookup_view(result));

            ctx.push(heap_primitive);

            Ok(())

        }

        ld_self(ctx, args) {
            let Some(name) = args.get(0) else {
                bail!("`ld_self` requires a name argument");
            };

            let var = ctx.load_local(name).with_context(|| format!("load before store (`{name}` not in this stack frame)\nframe `{:?}`'s variables:\n{:?}", ctx.rced_call_stack().borrow().get_frame_label(), ctx.get_frame_variables()))?;

            ctx.push_front(var.borrow().0.clone());

            Ok(())
        }

        assert(ctx, args) {
            if ctx.stack_size() != 1 {
                bail!("assert can only operate on a single item");
            }

            let item = ctx.pop().unwrap();

            let result = item.equals(&bool!(true))?;

            if !result {
                let start = &args[0];
                let end = &args[1];

                bail!("An explicit assertion (start: Pos {start}, end: Pos {end}) failed in this program.");
            }

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

        neq(ctx=ctx) {
            if ctx.stack_size() != 2 {
                bail!("equ requires only 2 items in the local stack");
            }

            let first = ctx.pop().unwrap();
            let second = ctx.pop().unwrap();

            let result = !first.equals(&second)?;

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
                ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));
            } else {
                ctx.signal(InstructionExitState::PushScope(SpecialScope::If));
            }

            Ok(())
        }

        while_loop(ctx, args) {
            if ctx.stack_size() == 0 {
                bail!("while statements require at least one entry in the local stack")
            }

            let item = ctx.pop().unwrap();
            ctx.clear_stack();

            let Primitive::Bool(b) = item else {
                bail!("while statement can only test booleans")
            };

            let Some(offset) = args.first() else {
                bail!("while statements require an argument to instruct where to jump if falsey")
            };

            /*
            if !b {
                ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));
            } else {
                ctx.signal(InstructionExitState::PushScope(SpecialScope::WhileLoop))
            }
             */

            if !b {
                ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));
            } else {
                ctx.signal(InstructionExitState::PushScope(SpecialScope::WhileLoop))
            }

            Ok(())
        }

        jmp(ctx, args) {
            let Some(offset) = args.first() else {
                bail!("jmp statements require an argument to instruct where to jump")
            };

            ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));

            Ok(())
        }

        jmp_pop(ctx, args) {
            let Some(offset) = args.first() else {
                bail!("jmp_pop statements require an argument to instruct where to jump")
            };

            let frames_to_pop = args.get(1).map_or_else(|| Ok(1), |frames_to_pop| frames_to_pop.parse::<usize>())?;

            ctx.signal(InstructionExitState::GotoPopScope(offset.parse::<isize>()?, frames_to_pop));

            Ok(())
        }

        done(ctx, _args) {
            log::trace!("DONE & POP");
            ctx.signal(InstructionExitState::PopScope);

            Ok(())
        }

        else_stmt(ctx, _args) {
            log::trace!("PUSH ELSE");

            ctx.signal(InstructionExitState::PushScope(SpecialScope::Else));

            Ok(())
        }
    }

    instruction! {
        call_lib(ctx, args) {
            let (Some(lib_name), Some(func_name)) = (args.get(0), args.get(1)) else {
                bail!("expected syntax: call_lib path/to/lib.dll function_name")
            };

            let arguments = ctx.get_local_operating_stack();

            ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                destination: JumpRequestDestination::Library {
                    lib_name: lib_name.clone(),
                    func_name: func_name.clone(),
                },
                // destination_label: JumpRequestDestination::Standard(first.clone()),
                callback_state: None,
                stack: ctx.rced_call_stack(),
                arguments,
            }));

            ctx.clear_stack();
            Ok(())
        }
    }
}

/// Get a function pointer to the bytecode instruction associated with a byte.
pub fn query(byte: u8) -> InstructionSignature {
    instruction_constants::FUNCTION_POINTER_LOOKUP[byte as usize]
}

/// Run an instruction.
#[inline(always)]
pub fn run_instruction(ctx: &mut Ctx, instruction: &Instruction) -> Result<()> {
    let instruction_fn = query(instruction.id);
    instruction_fn(ctx, &instruction.arguments)?;
    Ok(())
}

/// Parse a string into tokens based on preset rules.
///
/// Tokens can be space-delimited or comma-delimited. To indicate a token
/// contains a whitespace (`0x20`), the token must be wrapped with `"..."`.
///
/// This function supports encoding:
/// * `"\\n"` -> `"\n"`
/// * `"\\r"` -> `"\r"`
/// * `"\\t"` -> `"\t"`
///
/// # Errors
/// This function will error if its input is invalid.
/// Such cases arise from tokens that start with a `"` and do not close the quote.
/// Will also error if given an unknown escape sequence.
///
/// # Examples
///
/// ```ignore
/// let single = split_string("Hello".into());
/// println!("{single:?}"); // Ok(["Hello"])
///
/// let with_quotes = split_string("\"Hello\"".into());
/// assert_eq(single, with_quotes);
///
/// let multiple = split_string("Hello World".into());
/// println!("{multiple:?}"); // Ok(["Hello", "World"])
///
/// let combined = split_string("\"Hello World\"".into());
/// println!("{combined:?}"); // Ok(["Hello World"])
///
/// ```
pub fn split_string(string: Cow<str>) -> Result<Box<[String]>> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes && (char.is_whitespace()) {
            if !buf.is_empty() {
                result.push(buf.to_string());
                buf.clear();
            }
            continue;
        }

        match char {
            '\\' => {
                if escaping {
                    buf.push(char);
                }
                escaping = !escaping;
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
        if !buf.is_empty() {
            result.push(buf.to_string());
        }
        Ok(result.into_boxed_slice())
    }
}

/// If the program needs to jump, it must know if it is jumping
/// to a `.ms` file or a library. This enum represents that
/// destination.
#[derive(Clone, Debug)]
pub enum JumpRequestDestination {
    Standard(String),
    Library { lib_name: String, func_name: String },
}

/// This struct packs together data that represents the state of an
/// interpreter jump.
#[derive(Clone, Debug)]
pub struct JumpRequest {
    /// The place to which the request is going.
    pub destination: JumpRequestDestination,
    /// If this request is a closure or introduces a unique environment,
    /// this field will be `Some`.
    pub callback_state: Option<Rc<VariableMapping>>,
    /// This is a shared reference to the interpreter call stack.
    pub stack: Rc<RefCell<Stack>>,
    /// The arguments to the jump request. If this request is a function
    /// call (which should be 99% of cases), it will be the arguments passed
    /// from the caller.
    pub arguments: Vec<Primitive>,
}

/// A wrapper for a bytecode instruction.
/// In bytecode format, will look like:
///
/// `{BYTE} (SP {ARG})* NUL`
///
/// Where:
/// * BYTE = Instruction#id
/// * SP = 0x20
/// * NUL = 0x00
/// * ARG = "argument" | argument | "with spaces" | {LITERAL}
#[derive(Debug)]
pub struct Instruction {
    /// This instruction's identity. See the [static instruction array](crate::instruction_constants::FUNCTION_POINTER_LOOKUP)
    /// for valid identities.
    pub id: u8,
    /// The arguments to the instruction.
    arguments: Box<[String]>,
}

impl Instruction {
    /// Simple constructor
    pub const fn new(id: u8, arguments: Box<[String]>) -> Self {
        Instruction { id, arguments }
    }
}
