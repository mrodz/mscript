//! All the implementations for each bytecode instruction are found here.

use super::context::Ctx;
use super::function::InstructionExitState;
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
use std::sync::Mutex;

/// This variable allows instructions to register objects on the fly.
static mut OBJECT_BUILDER: Lazy<Mutex<ObjectBuilder>> =
    Lazy::new(|| Mutex::new(ObjectBuilder::new()));

/// Used for type declarations in lookup tables.
pub type InstructionSignature = fn(&mut Ctx, &[String]) -> Result<()>;

/// This submodule contains the implementations for
/// each instruction, and nothing more.
#[deny(dead_code)]
pub mod implementations {
    use super::*;
    use crate::context::SpecialScope;
    use crate::function::{PrimitiveFunction, ReturnValue};
    use crate::stack::flag_constants::READ_ONLY;
    use crate::stack::{PrimitiveFlagsPair, VariableFlags};
    use crate::variables::{GcMap, HeapPrimitive};
    use crate::{bool, function, int, object, optional, vector};
    use std::collections::HashMap;

    #[inline(always)]
    pub(crate) fn pop(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if !args.is_empty() {
            bail!("unexpected parameter")
        }

        ctx.pop();

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn stack_dump(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        println!("====== Start Context Dump ======");
        'get_data: {
            if let Some(arg0) = args.first() {
                if arg0 == "verbose" {
                    log::info!("Program Context = {ctx:?}");
                    break 'get_data;
                } else {
                    bail!("unknown debug argument ({arg0})")
                }
            }

            let stack = ctx.get_call_stack_string();

            println!("\nFunction: {}", ctx.owner());

            let mut operating_stack_str = String::new();

            for (idx, op_item) in ctx.get_local_operating_stack().iter().enumerate() {
                operating_stack_str += "\n\t[";
                operating_stack_str += &idx.to_string();
                operating_stack_str += "] ";
                operating_stack_str += &op_item.to_string();
            }

            println!("\nOperating Stack:{operating_stack_str}");
            println!("\nStack Trace:\n{stack}");
            println!(
                "\nThis Frame's Variables:\n\t{}\n",
                ctx.get_frame_variables()?
            );
        }
        println!("======= End Context Dump =======");

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn neg(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        let Some(val) = ctx.get_last_op_item_mut() else {
            bail!("neg requires one item on the local operating stack")
        };

        val.negate()?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn not(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        let Some(val) = ctx.get_last_op_item_mut() else {
            bail!("not requires one item on the local operating stack")
        };

        let Primitive::Bool(val) = val else {
            bail!("not can only negate booleans")
        };

        *val = !*val;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn bin_op(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        use Primitive::*;
        let symbols = args
            .first()
            .context("Expected an operation [+,-,*,/,%,>,>=,<,<=,=,&&,||,^,|,xor,&,<<,>>]")?;

        let (Some(right), Some(left)) = (ctx.pop(), ctx.pop()) else {
            bail!(
                "bin_op requires two items on the local operating stack (found {:?})",
                ctx.get_local_operating_stack()
            )
        };

        let left = left.move_out_of_heap_primitive()?;
        let right = right.move_out_of_heap_primitive()?;

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
            ("|", ..) => left | right,
            ("xor", ..) => std::ops::BitXor::bitxor(left, right),
            ("&", ..) => left & right,
            ("<<", ..) => left << right,
            (">>", ..) => left >> right,
            ("is", ..) => left.runtime_addr_check(&right),
            _ => bail!("unknown operation: {symbols} (got &{left}, &{right})"),
        }
        .context("invalid binary operation")?;

        ctx.clear_and_set_stack(result);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn bin_op_assign(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let op = args
            .first()
            .context("Expected an operation [+=,-=,*=,/=,%=]")?;

        if let Some(name) = args.get(1) {
            let bundle = ctx
                .load_variable(name)
                .with_context(|| format!("{name} has not been mapped"))?;
            let value: &mut Primitive = ctx
                .get_last_op_item_mut()
                .context("there must be a value at the top of the stack for a `bin_op_assign`")?;

            let result = {
                let no_hp = value.move_out_of_heap_primitive_borrow()?;
                let no_mut: &Primitive = &no_hp;

                match op.as_str() {
                    "+=" => (&*bundle.primitive() + no_mut)?,
                    "-=" => (&*bundle.primitive() - no_mut)?,
                    "*=" => (&*bundle.primitive() * no_mut)?,
                    "/=" => (&*bundle.primitive() / no_mut)?,
                    "%=" => (&*bundle.primitive() % no_mut)?,
                    _ => bail!("unknown assignment operation: {op}"),
                }
            };

            bundle.set_primitive(result.clone());
            *value = result;
        } else {
            let value = ctx
                .pop()
                .context("there must be a value at the top of the stack for a `bin_op_assign`")?
                .move_out_of_heap_primitive()?;

            let Some(maybe_ptr) = ctx.get_last_op_item_mut() else {
                bail!("`bin_op_assign` without a name argument will attempt to modify a pointer that is second to last on the stack, but no primitive was there");
            };

            let Primitive::HeapPrimitive(ptr) = maybe_ptr else {
                bail!("`bin_op_assign` tried to modify a pointer, but {maybe_ptr} is not a HeapPrimitive");
            };

            let result = ptr
                .update(|current| {
                    Ok(match op.as_str() {
                        "+=" => (current.deref() + &value)?,
                        "-=" => (current.deref() - &value)?,
                        "*=" => (current.deref() * &value)?,
                        "/=" => (current.deref() / &value)?,
                        "%=" => (current.deref() % &value)?,
                        _ => bail!("unknown assignment operation: {op}"),
                    })
                })?
                .to_owned();

            *maybe_ptr = result;
        };

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn ptr_mut(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        if ctx.stack_size() < 2 {
            bail!(
                "mutating a ptr requires [ptr, value] (found: {:?})",
                ctx.get_local_operating_stack()
            );
        }

        let new_val = ctx.pop().unwrap();
        let maybe_vec = ctx.pop().unwrap();

        let Primitive::HeapPrimitive(ref vec_ptr) = maybe_vec else {
            bail!("expected a mutable heap primitive, found {maybe_vec}");
        };

        vec_ptr.set(new_val)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn vec_op(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let mut arg_iter = args.iter();
        let op_name = arg_iter.next().context("Expected a vector operation")?;
        let bytes = op_name.as_bytes();

        if let [b'+', ..] = bytes {
            if ctx.stack_size() != 1 {
                bail!("vec_op +push operations require only a single item on the operating stack")
            }

            let new_val = ctx.pop().unwrap();

            let primitive_with_flags: PrimitiveFlagsPair = ctx
                .load_local(&op_name[1..])
                .context("vector not found for pushing")?;

            let Primitive::Vector(ref vector) = &*primitive_with_flags.primitive() else {
                bail!("not a vector, trying to push")
            };

            vector.0.borrow_mut().push(new_val);
        } else if let [b'[', index @ .., b']'] = bytes {
            let Some(indexable) = ctx.pop() else {
                bail!("the stack is empty");
            };

            let indexable = indexable.move_out_of_heap_primitive()?;

            // this manual byte slice to usize conversion is more performant
            let idx: usize = 'index_gen: {
                let mut idx = 0;
                let mut max = 10_usize.pow(index.len() as u32 - 1);

                for byte in index {
                    if !byte.is_ascii_digit() {
                        let index_as_str = std::str::from_utf8(index)?;
                        let variable = ctx.load_local(index_as_str).with_context(|| format!("'{index_as_str}' is not a literal number nor a name that has been mapped locally"))?;

                        let as_index: usize = variable.primitive().try_into_numeric_index()?;

                        break 'index_gen as_index;
                    }
                    idx += (byte - b'0') as usize * max;
                    max /= 10;
                }

                idx
            };

            match indexable {
                Primitive::Vector(ref vector_shared) => {
                    {
                        let len = vector_shared.0.borrow().len();
                        if idx >= len {
                            bail!("index {idx} out of bounds (len {len})")
                        }
                    }

                    let heap_primitive = HeapPrimitive::new_array_view(vector_shared.clone(), idx);

                    ctx.push(Primitive::HeapPrimitive(heap_primitive));
                }
                Primitive::Str(ref string) => {
                    let mut str_chars = string.chars();
                    ctx.push(Primitive::Str(
                        str_chars
                            .nth(idx)
                            .with_context(|| {
                                format!(
                                    "index {idx} out of bounds (len {} chars, {} bytes)",
                                    string.chars().count(),
                                    string.len()
                                )
                            })?
                            .to_string(),
                    ))
                }
                other => bail!("Cannot perform a vector operation on a non-vector: {other:?}"),
            }
        } else {
            match op_name.as_str() {
                "reverse" => {
                    let Some(Primitive::Vector(vector)) = ctx.get_last_op_item() else {
                        bail!(
                            "Cannot perform a vector operation on a non-vector (found: {:?})",
                            ctx.get_last_op_item()
                        )
                    };

                    let mut vector = vector.0.borrow_mut();

                    vector.reverse();
                }
                "mut" => {
                    let idx = arg_iter
                        .next()
                        .context("mutating an array requires an argument")?;
                    let idx = idx.parse::<usize>()?;

                    if ctx.stack_size() != 2 {
                        bail!("mutating an array requires two items in the local operating stack")
                    }

                    let new_item = ctx.pop().context("could not pop first item")?;

                    let Some(primitive) = ctx.get_last_op_item() else {
                        bail!("Stack is empty")
                    };

                    let primitive = primitive.move_out_of_heap_primitive_borrow()?;

                    let Primitive::Vector(vector) = primitive.as_ref() else {
                        bail!("Cannot perform a vector operation on a non-vector (found: {primitive})")
                    };

                    let mut vector = vector.0.borrow_mut();

                    vector[idx] = new_item;
                }
                not_found => bail!("operation not found: `{not_found}`"),
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn nop(_: &mut Ctx, _: &[String]) -> Result<()> {
        bail!("nop/0x00 (null) instructions are usually signs of the runtime incorrectly loading a function. This instruction will never be emitted by the compiler")
    }

    #[inline(always)]
    pub(crate) fn void(ctx: &mut Ctx, _: &[String]) -> Result<()> {
        ctx.clear_stack();
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn breakpoint(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if cfg!(feature = "skip_breakpoint") {
            return Ok(());
        }

        let maybe_name = args.first();

        print!(
            "[!!] BREAKPOINT{}\n[!!] options\n[!!] - continue\n[!!] - dump\n[!!] Enter Option: ",
            maybe_name.map(|x| " ".to_owned() + x).unwrap_or_default()
        );

        let mut buf = String::new();
        stdout().flush()?;
        stdin().read_line(&mut buf)?;

        match buf.trim_end() {
            "continue" => Ok(()),
            "dump" => stack_dump(ctx, &[]),
            buf => {
                println!("[!!]\n[!!] BREAKPOINT\n[!!] '{buf}' is not a valid option.\n[!!]");
                breakpoint(ctx, args)
            }
        }
    }

    #[inline(always)]
    pub(crate) fn ret(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
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

    #[inline(always)]
    pub(crate) fn ret_mod(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        if ctx.stack_size() != 0 {
            bail!("ret_mod should have a clean operating stack");
        }

        let module = ctx.get_file_module();

        ctx.signal(InstructionExitState::ReturnValue(ReturnValue::Value(
            module,
        )));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_bool(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("expected 1 parameter")
        }

        let var = Primitive::make_bool(&args[0])?;

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_int(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("expected 1 parameter")
        }

        let var = Primitive::make_int(&args[0])?;

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_float(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("expected 1 parameter")
        }

        let var = Primitive::make_float(&args[0])?;

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_byte(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("expected 1 parameter")
        }

        let var = Primitive::make_byte(&args[0])?;

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_bigint(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("expected 1 parameter")
        }

        let var = Primitive::make_bigint(&args[0])?;

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_str(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let raw_str = match args.len() {
            0 => "",
            1 => &args[0],
            _ => bail!("make_str requires 0 arguments (empty string) or one argument (the string), but got {args:?}"),
        };

        let var = crate::string!(raw raw_str);

        ctx.push(var);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_function(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(location) = args.first() else {
            bail!("making a function pointer requires a path to find it")
        };

        let len = args.len();
        let callback_state = if len != 1 {
            let mut arguments = HashMap::with_capacity(len - 1); // maybe len
            for var_name in &args[1..] {
                let var = if let Some(var) = ctx.load_variable(var_name) {
                    var
                } else if let Ok(var) = ctx.load_callback_variable(var_name) {
                    var
                } else {
                    bail!("{var_name} is not in scope")
                };

                arguments.insert(var_name.clone(), var.clone());
            }

            Some(arguments.into())
        } else {
            None
        };

        let function_ptr = PrimitiveFunction::new(location.into(), callback_state);

        ctx.push(function!(function_ptr));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_object(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if !args.is_empty() {
            bail!("`make_object` does not require arguments")
        }

        let object_variables = {
            let frame_variables = ctx.get_frame_variables()?;
            VariableMapping::clone(&frame_variables)
        };

        let function = ctx.owner();
        let name = function.name();

        let obj = unsafe {
            let mut lock = OBJECT_BUILDER.lock().unwrap();

            if !lock.has_class_been_registered(name) {
                let location = &function.location();
                let object_path = format!("{}#{name}$", location.upgrade().unwrap().path());

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

                lock.register_class(name.to_owned(), mapping);
            }

            lock.name(name.to_owned())
                .object_variables(object_variables)
                .build()
        };

        ctx.push(object!(obj));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn make_vector(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() > 1 {
            bail!("`make_vector` instruction requires 1 argument (capacity) or none (initializes with contents of local operating stack)")
        }

        let Some(arg) = args.first() else {
            let vec = ctx.get_local_operating_stack().clone();

            ctx.clear_stack();
            ctx.push(vector!(raw vec));

            return Ok(());
        };

        let capacity = arg
            .parse::<usize>()
            .context("argument must be of type usize")?;

        let vec = vector!(raw Vec::with_capacity(capacity));

        ctx.push(vec);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn printn(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(arg) = args.first() else {
            bail!("expected 1 parameter (index into local operating stack), or * to print all");
        };

        if arg == "*" {
            let Some(first) = ctx.get_nth_op_item(0) else {
                println!();
                return Ok(());
            };

            log::warn!(
                "The `printn` instruction should not be used. Favor the standard library instead."
            );

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

        let arg = arg
            .parse::<usize>()
            .context("argument must be of type usize")?;

        println!("{}", ctx.get_nth_op_item(arg).context("nothing at index")?);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn call_object(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let path = args.last().context("missing method name argument")?;

        let Some(first) = ctx.get_nth_op_item(0) else {
            bail!("there is no item in the local stack")
        };

        let Primitive::Object(o) = first else {
            bail!("last item in the local stack {first:?} is not an object.")
        };

        let callback_state = Some(o.object_variables.clone());

        let arguments = ctx.get_local_operating_stack().clone();
        ctx.clear_stack();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Standard(path.clone()),
            callback_state,
            stack: ctx.rced_call_stack(),
            arguments,
        }));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn mutate(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let var_name = args
            .first()
            .context("object mutation requires a name argument")?;

        if ctx.stack_size() != 2 {
            bail!("mutating an object requires two items in the local operating stack (obj, data)")
        }

        let new_item: Primitive = ctx.pop().context("could not pop first item")?;

        let Some(Primitive::Object(o)) = ctx.get_last_op_item() else {
            bail!("Cannot perform an object mutation on a non-object")
        };

        let obj_view = o;
        let has_variable = obj_view
            .has_variable(var_name)
            .context("variable does not exist on object")?;

        let ty_lhs = has_variable.primitive().ty();
        let ty_rhs = new_item.ty();

        if ty_lhs != ty_rhs {
            bail!("mismatched types in assignment ({ty_lhs:?} & {ty_rhs:?})")
        }

        has_variable.set_primitive(new_item);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn call(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(first) = args.first() else {
            let Some(last) = ctx.pop() else {
                bail!("missing argument, and the local stack is empty");
            };

            match last.move_out_of_heap_primitive()? {
                Primitive::Function(ref f) => {
                    let destination = JumpRequestDestination::Standard(f.location().to_owned());

                    let callback_state = f.callback_state().clone();

                    ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
                        destination,
                        callback_state,
                        stack: ctx.rced_call_stack(),
                        arguments: ctx.get_local_operating_stack().clone(),
                    }));

                    ctx.clear_stack();

                    return Ok(());
                }
                Primitive::BuiltInFunction(ref variant) => {
                    ctx.add_frame(Cow::Owned(format!("<native code>#{variant:?}")));
                    match variant.run(ctx).with_context(|| format!("built-in function raised an exception: {variant:?}()"))? {
                        (Some(primitive), None) => {
                            ctx.clear_and_set_stack(primitive);
                        }
                        (None, Some(bridge)) => {
                            ctx.signal(InstructionExitState::BeginNotificationBridge(bridge));
                        }
                        (None, None) => {
                            ctx.clear_stack();
                        }
                        (Some(primitive), Some(bridge)) => {
                            unimplemented!("primitive and bridge combined is not supported: ({primitive:?}, {bridge:?})")
                        }
                    }
                    ctx.pop_frame();

                    return Ok(());
                }
                last => bail!("missing argument, and the last item in the local stack ({last:?}, S:{:#?}) is not a function.", ctx.get_local_operating_stack()),
            }
        };

        let arguments = ctx.get_local_operating_stack().clone();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Standard(first.clone()),
            callback_state: None,
            stack: ctx.rced_call_stack(),
            arguments,
        }));

        ctx.clear_stack();

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn call_self(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        let arguments = ctx.get_local_operating_stack().clone();

        let callback_state = ctx.get_callback_variables();

        let stack = ctx.rced_call_stack();

        let name = {
            let stack_view = stack.borrow();
            stack_view
                .get_executing_function_label()
                .context("not run in a function")?
                .to_owned()
        };

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Standard(name),
            callback_state,
            stack,
            arguments,
        }));

        ctx.clear_stack();

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn module_entry(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(first) = args.first() else {
            bail!("expected one argument");
        };

        let arguments = ctx.get_local_operating_stack().clone();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Module(first.clone()),
            callback_state: None,
            stack: ctx.rced_call_stack(),
            arguments,
        }));

        ctx.clear_stack();

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn split_lookup_store(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(top) = ctx.get_last_op_item() else {
            bail!("`split_lookup_store` expected an item at the top of the operating stack");
        };

        match top {
            Primitive::Module(module) => {
                let module = module.clone(); // to please borrow checker
                let view = module.borrow();
                // let view = module;

                for name in args {
                    let Some(bundle) = view.get(name) else {
                        bail!("{name} does not exist on {view}");
                    };

                    ctx.register_variable_local(name.to_owned(), bundle.primitive().clone())?;
                }
            }
            unknown => bail!("`split_lookup_store` cannot be used on {unknown}"),
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn arg(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(first) = args.first() else {
            bail!("expected one argument")
        };

        let n = first
            .parse::<usize>()
            .context("argument must be of type usize")?;

        let Some(nth_arg) = ctx.nth_arg(n) else {
            bail!("#{n} argument does not exist (range 0..{})", ctx.argc())
        };

        ctx.push(nth_arg.clone()); // 4/4/2023: why do we need to clone here? maybe re-work this.

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn stack_size(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        let size = int!(ctx.frames_count().try_into()?);
        ctx.push(size);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn reserve_primitive(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        ctx.push(optional!(empty));
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn unwrap_into(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("`unwrap_into` requires a name");
        };

        let Some(primitive) = ctx.pop() else {
            bail!("`unwrap_into` requires a primitive at the top of the local operating stack");
        };

        let primitive = primitive.move_out_of_heap_primitive()?;

        let status = match primitive {
            Primitive::Optional(Some(ref unwrapped)) => {
                let var = unwrapped.as_ref().to_owned();

                let var = var.move_out_of_heap_primitive()?;

                ctx.register_variable_local(name.to_owned(), var)?;
                true
            }
            primitive @ Primitive::Optional(None) => {
                let primitive = primitive.move_out_of_heap_primitive()?;

                ctx.register_variable_local(name.to_owned(), primitive)?;
                false
            }
            other_primitive => {
                let other_primitive = other_primitive.move_out_of_heap_primitive()?;

                ctx.register_variable_local(name.to_owned(), other_primitive)?;
                true
            }
        };

        ctx.push(bool!(status));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn unwrap(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(primitive) = ctx.get_last_op_item_mut() else {
            bail!("`unwrap` requires a primitive at the top of the local operating stack");
        };

        if let Primitive::Optional(optional) = primitive
            .move_out_of_heap_primitive_borrow()
            .context("could not move out of heap primitive")?
            .as_ref()
        {
            if let Some(new_primitive) = optional {
                *primitive = *new_primitive.clone();
            } else {
                let span = args.first().map(String::as_str);
                bail!(
                    "LOGIC ERROR IN CODE >> {}: unwrap of `nil`",
                    span.unwrap_or("<no details>")
                );
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn jmp_not_nil(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(primitive) = ctx.get_last_op_item() else {
            bail!("`jmp_not_nil` requires a primitive at the top of the local operating stack");
        };

        let Some(lines_to_jump) = args.first() else {
            bail!("`jmp_not_nil` requires lines_to_jump");
        };

        let lines_to_jump = lines_to_jump
            .parse::<isize>()
            .context("jmp_not_nil needs lines_to_jump: isize")?;

        if let Primitive::Optional(None) = primitive
            .move_out_of_heap_primitive_borrow()
            .context("could not move out of heap primitive")?
            .as_ref()
        {
            ctx.pop();
            return Ok(());
        }

        ctx.signal(InstructionExitState::Goto(lines_to_jump));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn store(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("`store` requires a name")
        };

        if ctx.stack_size() != 1 {
            bail!(
                "`store` can only store a single item (found: {:?})",
                ctx.get_local_operating_stack()
            );
        }

        let arg = ctx.pop().unwrap();

        let arg = arg.move_out_of_heap_primitive()?;

        ctx.register_variable(Cow::Owned(name.to_owned()), arg)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn export_name(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("`export_name` requires a name")
        };

        let pair = ctx
            .load_local(name)
            .with_context(|| format!("`{name}` is not in scope and cannot be exported"))?;

        log::trace!("exporting: {name} = {pair:?}");

        ctx.register_export(name.to_owned(), pair)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn load_self_export(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(src) = args.first() else {
            bail!("`load_self_export` requires a source name")
        };

        let bundle = ctx
            .load_self_export(src)
            .with_context(|| format!("`{src}` has not been exported from the executing module."))?;

        let primitive = bundle.primitive().clone();

        ctx.push(primitive);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn export_special(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("`export_special` requires a name")
        };

        let export_name = args.get(1).unwrap_or(name);

        if ctx.stack_size() != 1 {
            bail!(
                "`export_special` can only store a single item (found: {:?})",
                ctx.get_local_operating_stack()
            );
        }

        let arg = ctx.pop().unwrap();

        let arg = arg.move_out_of_heap_primitive()?;

        let variable = PrimitiveFlagsPair::new(arg, VariableFlags(READ_ONLY));

        ctx.register_export(export_name.to_owned(), variable.clone())?;
        ctx.ref_variable(Cow::Owned(name.to_owned()), variable);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn store_fast(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("store_fast requires a name")
        };

        if ctx.stack_size() != 1 {
            bail!("store_fast can only store a single item");
        }

        let arg = ctx.pop().unwrap();

        let arg = arg.move_out_of_heap_primitive()?;

        ctx.register_variable_local(name.clone(), arg)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn store_object(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("store_object requires a name")
        };

        if ctx.stack_size() != 1 {
            bail!("store_object can only store a single item");
        }

        let arg = ctx.pop().unwrap();

        let arg = arg.move_out_of_heap_primitive()?;

        ctx.update_callback_variable(name, arg)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn store_skip(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some([name, predicate, lines_to_jump]) = args.get(0..=2) else {
            bail!("store_skip: name:str predicate:u8(1/0) lines_to_jump:isize")
        };

        let predicate = predicate
            .parse::<u8>()
            .context("store_skip needs predicate: u8")?;
        let lines_to_jump = lines_to_jump
            .parse::<isize>()
            .context("store_skip needs lines_to_jump: isize")?;

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
                return Ok(());
            }
        } else {
            // skip if false
            if !val {
                ctx.signal(InstructionExitState::Goto(lines_to_jump));
                return Ok(());
            }
        }

        let arg = ctx.pop().unwrap();

        let arg = arg.move_out_of_heap_primitive()?;
        ctx.register_variable_local(name.clone(), arg)?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn fast_rev2(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
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

    #[inline(always)]
    pub(crate) fn load(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("load requires a name")
        };

        let var = if let Some(var) = ctx.load_variable(name) {
            var
        } else if let Ok(var) = ctx.load_callback_variable(name) {
            var
        } else {
            bail!("load before store (`{name}` not in scope)")
        };

        ctx.push(var.primitive().clone());

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn load_fast(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("load requires a name")
        };

        let var = ctx.load_local(name).with_context(|| format!("load before store (`{name}` not in this stack frame)\nframe `{:?}`'s variables:\n{:?}", ctx.rced_call_stack().borrow().get_frame_label(), ctx.get_frame_variables()))?;

        ctx.push(var.primitive().clone());

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn load_callback(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("loading a callback variable requires one parameter: (name)")
        };

        let var = ctx.load_callback_variable(name)?;

        ctx.push(var.primitive().clone());

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn delete_name_scoped(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.is_empty() {
            bail!("delete_name_scoped requires names to delete")
        };

        for name in args {
            ctx.delete_variable_local(name)?;
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn delete_name_reference_scoped(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if args.len() != 1 {
            bail!("delete_name_reference_scoped can only delete to retrieve one name");
        }

        let name = args.first().unwrap();

        let deleted: PrimitiveFlagsPair = ctx.delete_variable_local(name)?;
        let primitive: Primitive = deleted.primitive().clone();

        ctx.push(primitive);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn lookup(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if ctx.stack_size() != 1 {
            bail!(
                "`lookup` requires a single item on the stack (found {:?})",
                ctx.get_local_operating_stack()
            );
        }

        let Some(name) = args.first() else {
            bail!("`lookup` requires a name argument");
        };

        let primitive = ctx.pop().unwrap();

        let result = match primitive.lookup(name)? {
            Ok(result) => result,
            Err(Primitive::Optional(None)) => {
                bail!("LOGIC ERROR IN CODE >> nil object, looking up `{name}`")
            }
            Err(primitive) => bail!("`{name}` does not exist on `{primitive}`"),
        };

        let heap_primitive = Primitive::HeapPrimitive(HeapPrimitive::new_lookup_view(result));

        ctx.push(heap_primitive);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn ld_self(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(name) = args.first() else {
            bail!("`ld_self` requires a name argument");
        };

        let var = ctx.load_local(name).with_context(|| format!("load before store (`{name}` not in this stack frame)\nframe `{:?}`'s variables:\n{:?}", ctx.rced_call_stack().borrow().get_frame_label(), ctx.get_frame_variables()))?;

        ctx.push_front(var.primitive().clone());

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn assert(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        if ctx.stack_size() != 1 {
            bail!("assert can only operate on a single item");
        }

        let item = ctx.pop().unwrap();

        let result = item.equals(&bool!(true))?;

        if !result {
            let span = &args[0];

            bail!("An explicit assertion failed in this program ({span})");
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn equ(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        if ctx.stack_size() != 2 {
            bail!("equ requires only 2 items in the local stack")
        }

        let first = ctx.pop().unwrap().move_out_of_heap_primitive()?;
        let second = ctx.pop().unwrap().move_out_of_heap_primitive()?;

        let result = first.equals(&second)?;

        ctx.push(bool!(result));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn neq(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        if ctx.stack_size() != 2 {
            bail!("neq requires only 2 items in the local stack");
        }

        let first = ctx.pop().unwrap().move_out_of_heap_primitive()?;
        let second = ctx.pop().unwrap().move_out_of_heap_primitive()?;

        let result = !first.equals(&second)?;

        ctx.push(bool!(result));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn if_stmt(ctx: &mut Ctx, args: &[String]) -> Result<()> {
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

    #[inline(always)]
    pub(crate) fn while_loop(ctx: &mut Ctx, args: &[String]) -> Result<()> {
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

        if !b {
            ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));
        } else {
            ctx.signal(InstructionExitState::PushScope(SpecialScope::WhileLoop))
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn jmp(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(offset) = args.first() else {
            bail!("jmp statements require an argument to instruct where to jump")
        };

        ctx.signal(InstructionExitState::Goto(offset.parse::<isize>()?));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn jmp_pop(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let Some(offset) = args.first() else {
            bail!("jmp_pop statements require an argument to instruct where to jump")
        };

        let frames_to_pop = args
            .get(1)
            .map_or_else(|| Ok(1), |frames_to_pop| frames_to_pop.parse::<usize>())?;

        ctx.signal(InstructionExitState::GotoPopScope(
            offset.parse::<isize>()?,
            frames_to_pop,
        ));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn done(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        log::trace!("DONE & POP");
        ctx.signal(InstructionExitState::PopScope);

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn else_stmt(ctx: &mut Ctx, _args: &[String]) -> Result<()> {
        log::trace!("PUSH ELSE");

        ctx.signal(InstructionExitState::PushScope(SpecialScope::Else));

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn call_lib(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let (Some(lib_name), Some(func_name)) = (args.first(), args.get(1)) else {
            bail!("expected syntax: call_lib path/to/lib.dll function_name")
        };

        let arguments = ctx.get_local_operating_stack().clone();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: JumpRequestDestination::Library {
                lib_name: lib_name.clone(),
                func_name: func_name.clone(),
            },
            callback_state: None,
            stack: ctx.rced_call_stack(),
            arguments,
        }));

        ctx.clear_stack();
        Ok(())
    }

    #[inline(always)]
    pub fn make_map(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        /*
         * The user can misbehave all they want, but that is not our concern.
         */
        #[allow(clippy::mutable_key_type)]
        let raw_map = if let Some(first) = args.first() {
            let capacity =
                str::parse::<usize>(first).context("could not parse usize for map capacity")?;
            HashMap::with_capacity(capacity)
        } else {
            HashMap::new()
        };

        ctx.push(Primitive::Map(GcMap::new(raw_map)));

        Ok(())
    }

    #[inline(always)]
    pub fn fast_map_insert(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let (Some(map_register), Some(key_register)) = (args.first(), args.get(1)) else {
            bail!("fast map insert missing key register");
        };

        let map = ctx.load_local(map_register)?;

        let Primitive::Map(map) = &*map.primitive() else {
            bail!("inserting on a non-map")
        };

        let key = ctx.load_local(key_register)?;

        map.insert(
            key.primitive().clone(),
            ctx.pop().expect("no value in the op stack"),
        )?;

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn map_op(ctx: &mut Ctx, args: &[String]) -> Result<()> {
        let map_register = args.first().context("no map register arg")?;

        let index_key = ctx.pop().context("no index in the stack")?;

        let map = ctx
            .load_local(map_register)
            .with_context(|| format!("no map at register {map_register}"))?;

        let Primitive::Map(map) = &*map.primitive() else {
            bail!("{} is not a map", map.primitive())
        };

        ctx.push(Primitive::HeapPrimitive(HeapPrimitive::MapPtr(
            map.clone(),
            Box::new(index_key),
        )));

        Ok(())
    }
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
pub fn split_string(string: &str) -> Result<Box<[String]>> {
    split_string_v2(string, true)
}

/// Choose whether the output is one string, or if spaces/quotes
/// are delimiters for many outputs.
pub fn split_string_v2(string: &str, multi_target: bool) -> Result<Box<[String]>> {
    let mut result = Vec::<String>::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes && (char.is_whitespace()) {
            if multi_target {
                if !buf.is_empty() {
                    result.push(buf.to_string());
                    buf.clear();
                }
            } else {
                buf.push(char);
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

                if multi_target && in_quotes {
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
                    bail!("Unknown escape sequence: \\{char} (src = {string:?})")
                }
            }
        }

        buf.push(char);
    }

    if in_quotes {
        bail!("found EOL while parsing string: `{string}`")
    } else {
        if !buf.is_empty() {
            // flush buffer at the end of processing
            result.push(buf.to_string());
        }

        if result.is_empty() {
            // edge case
            result.push(String::new());
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
    Module(String),
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
    pub callback_state: Option<VariableMapping>,
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
    pub(crate) arguments: Box<[String]>,
}

impl Instruction {
    /// Simple constructor
    pub const fn new(id: u8, arguments: Box<[String]>) -> Self {
        Instruction { id, arguments }
    }
}
