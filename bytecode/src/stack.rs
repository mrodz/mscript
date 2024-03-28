//! Program call stack

use anyhow::{anyhow, bail, Context, Result};
use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use crate::context::SpecialScope;
use crate::function::BuiltInFunction;

use self::flag_constants::READ_ONLY;

use super::variables::Primitive;

/// Bit fields for variable flags.
pub(crate) mod flag_constants {
    pub const READ_ONLY: u8 = 0b00000001;
    pub const LOCAL_FRAME_ONLY: u8 = 0b00000100;
    pub const LOOP_VARIABLE: u8 = 0b00001000;
}

#[derive(Clone, Trace, Finalize)]
struct TupleWithGcOpt(Primitive, #[unsafe_ignore_trace] VariableFlags);

#[derive(Clone, Trace, Finalize)]
pub struct PrimitiveFlagsPair(Gc<GcCell<TupleWithGcOpt>>);

static PRIMITIVE_MODULE_MEMBER_FLAGS: VariableFlags = VariableFlags(READ_ONLY);

macro_rules! make_compiler_builtin {
    ($ident:expr) => {
        Arc::new(Mutex::new(PrimitiveFlagsPair(Gc::new(GcCell::new(
            TupleWithGcOpt(
                Primitive::BuiltInFunction($crate::NonSweepingBuiltInFunction($ident)),
                PRIMITIVE_MODULE_MEMBER_FLAGS,
            ),
        )))))
    };
}

pub static PRIMITIVE_MODULE: Lazy<PrimitiveModule> = Lazy::new(PrimitiveModule::new);

macro_rules! static_module_generator {
    ($($variant:ident($type:expr)),+ $(,)?) => {
        pub struct PrimitiveModule {
            $(
                $variant: Arc<Mutex<PrimitiveFlagsPair>>,
            )*
        }

        /*
         * Safe because:
         * 1. `PrimitiveModule` will never be written to outside of
         *    the `Lazy::new()` generator -- no race conditions
         * 2. While `Primitive` is not Send or Sync, the only variant that this
         *    module will ever create is `Primitive::BuiltinFunction`, which **is**
         *    Send + Sync. The compiler doesn't know this, though.
         */
        unsafe impl Send for PrimitiveModule {}
        unsafe impl Sync for PrimitiveModule {}

        impl PrimitiveModule {
            $(
                pub fn $variant(&self) -> PrimitiveFlagsPair {
                    return self.$variant.lock().unwrap().clone()
                }
            )*

            pub fn new() -> Self {
                Self {
                    $(
                        $variant: make_compiler_builtin!($type),
                    )*
                }
            }
        }
    };
}

static_module_generator! {
    vector_len(BuiltInFunction::VecLen),
    vector_reverse(BuiltInFunction::VecReverse),
    vector_inner_capacity(BuiltInFunction::VecInnerCapacity),
    vector_ensure_inner_capacity(BuiltInFunction::VecEnsureInnerCapacity),
    vector_map(BuiltInFunction::VecMap),
    vector_filter(BuiltInFunction::VecFilter),
    vector_remove(BuiltInFunction::VecRemove),
    vector_push(BuiltInFunction::VecPush),
    vector_join(BuiltInFunction::VecJoin),
    vector_index_of(BuiltInFunction::VecIndexOf),
    fn_is_closure(BuiltInFunction::FnIsClosure),
    generic_to_str(BuiltInFunction::GenericToStr),
    str_len(BuiltInFunction::StrLen),
    str_substring(BuiltInFunction::StrSubstring),
    str_contains(BuiltInFunction::StrContains),
    str_index_of(BuiltInFunction::StrIndexOf),
    str_inner_capacity(BuiltInFunction::StrInnerCapacity),
    str_reverse(BuiltInFunction::StrReverse),
    str_insert(BuiltInFunction::StrInsert),
    str_replace(BuiltInFunction::StrReplace),
    str_delete(BuiltInFunction::StrDelete),
    str_parse_int(BuiltInFunction::StrParseInt),
    str_parse_bigint(BuiltInFunction::StrParseBigint),
    str_parse_int_radix(BuiltInFunction::StrParseIntRadix),
    str_parse_bigint_radix(BuiltInFunction::StrParseBigintRadix),
    str_parse_bool(BuiltInFunction::StrParseBool),
    str_parse_float(BuiltInFunction::StrParseFloat),
    str_parse_byte(BuiltInFunction::StrParseByte),
    str_split(BuiltInFunction::StrSplit),
    generic_num_pow(BuiltInFunction::GenericPow),
    generic_num_powf(BuiltInFunction::GenericPowf),
    generic_num_sqrt(BuiltInFunction::GenericSqrt),
    generic_num_to_int(BuiltInFunction::GenericToInt),
    generic_num_to_bigint(BuiltInFunction::GenericToBigint),
    generic_num_to_byte(BuiltInFunction::GenericToByte),
    generic_num_to_float(BuiltInFunction::GenericToFloat),
    generic_num_abs(BuiltInFunction::GenericAbs),
    byte_to_ascii(BuiltInFunction::ByteToAscii),
    float_fpart(BuiltInFunction::FloatFPart),
    float_ipart(BuiltInFunction::FloatIPart),
    float_round(BuiltInFunction::FloatRound),
    float_floor(BuiltInFunction::FloatFloor),
    float_ceil(BuiltInFunction::FloatCeil),

}

impl PrimitiveFlagsPair {
    pub fn new(primitive: Primitive, flags: VariableFlags) -> Self {
        Self(Gc::new(GcCell::new(TupleWithGcOpt(primitive, flags))))
    }

    pub fn primitive(&self) -> GcCellRef<Primitive> {
        GcCellRef::map(self.0.borrow(), |x| &x.0)
        // unsafe { &(*self.0).0 }
    }

    pub fn flags(&self) -> GcCellRef<VariableFlags> {
        GcCellRef::map(self.0.borrow(), |x| &x.1)
        // unsafe { &(*self.0.get()).1 }
    }

    pub fn set_primitive(&self, new_value: Primitive) -> Primitive {
        // let ptr = self.0.get();
        // let ptr = unsafe { &mut (*ptr).0 };
        // std::mem::replace(ptr, new_value)
        let mut mutable_gc_view = self.0.borrow_mut();
        std::mem::replace(&mut mutable_gc_view.0, new_value)
    }

    pub fn update_primitive(
        &self,
        setter: impl FnOnce(&dyn Deref<Target = Primitive>) -> Result<Primitive>,
    ) -> Result<GcCellRef<Primitive>> {
        let new_value = setter(&self.primitive())?;
        self.set_primitive(new_value);
        Ok(self.primitive())
    }

    pub fn set_flags(&self, new_value: VariableFlags) -> VariableFlags {
        std::mem::replace(&mut self.0.borrow_mut().1, new_value)
    }
}

impl PartialEq for PrimitiveFlagsPair {
    fn eq(&self, other: &Self) -> bool {
        self.primitive().eq(&*other.primitive()) && self.flags().eq(&*other.flags())
    }
}

impl Debug for PrimitiveFlagsPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PrimitiveFlagsPair")
            .field(&*self.primitive())
            .field(&*self.flags())
            .finish()
    }
}

#[derive(PartialEq, Clone, Copy)]
pub struct VariableFlags(pub(crate) u8);

impl VariableFlags {
    pub const fn none() -> Self {
        VariableFlags(0)
    }

    pub const fn is_read_only(&self) -> bool {
        self.0 & flag_constants::READ_ONLY == flag_constants::READ_ONLY
    }

    pub const fn can_update(&self) -> bool {
        !self.is_read_only()
    }

    pub const fn is_exclusive_to_frame(&self) -> bool {
        self.0 & flag_constants::LOCAL_FRAME_ONLY == flag_constants::LOCAL_FRAME_ONLY
    }

    pub const fn is_loop_variable(&self) -> bool {
        self.0 & flag_constants::LOOP_VARIABLE == flag_constants::LOOP_VARIABLE
    }

    pub const fn bits(&self) -> u8 {
        self.0
    }
}

impl Debug for VariableFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = vec![];

        if self.0 & flag_constants::READ_ONLY == flag_constants::READ_ONLY {
            buffer.push("READ_ONLY")
        }

        write!(f, "{buffer:?}")
    }
}

#[derive(Default, Debug, PartialEq, Trace, Finalize, Clone)]
pub struct VariableMapping(HashMap<String, PrimitiveFlagsPair>);

impl Display for VariableMapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = vec![];

        for (key, value) in self.0.iter() {
            result.push(format!(
                "\n\t`{key}` = {} (attr: {:?})",
                value.primitive(),
                value.flags()
            ));
        }

        let string = if result.is_empty() {
            "None".into()
        } else {
            let mut ret = String::new();

            ret.push_str(&result[0]);

            for combo in &result[1..] {
                ret.push_str(", ");
                ret.push_str(combo);
            }

            ret
        };

        write!(f, "{string}")
    }
}

impl From<HashMap<String, PrimitiveFlagsPair>> for VariableMapping {
    fn from(value: HashMap<String, PrimitiveFlagsPair>) -> Self {
        Self(value)
    }
}

impl VariableMapping {
    pub fn get(&self, key: &str) -> Option<PrimitiveFlagsPair> {
        self.0.get(key).cloned()
    }

    /// Explicit clone, because this is probably not intented behavior.
    pub fn clone(instance: &Self) -> Self {
        Self(instance.0.clone())
    }

    pub fn contains(&self, key: &str) -> bool {
        self.0.contains_key(key)
    }

    pub fn update_once(&mut self, name: String, value: PrimitiveFlagsPair) -> Result<()> {
        if let Some(export) = self.0.insert(name, value) {
            bail!("value already present: {export:?}");
        }

        Ok(())
    }

    /// Update the value of a variable that has already been registered.
    /// The variable will keep **ALL** of the flags from its previous mapping.
    ///
    /// # Errors
    /// This function can error if the variable has not been mapped, or if the name is read-only.
    pub fn update(&self, name: &str, value: Primitive) -> Result<()> {
        // if insert returns None, that means there was no value there,
        // and this `update` is invalid.
        if let Some(pair) = self.0.get(name) {
            if pair.flags().can_update() {
                pair.set_primitive(value);
            } else {
                bail!("variable is read-only")
            }
        } else {
            bail!("variable has not been mapped")
        }

        Ok(())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

/// A stack frame in the MScript interpreter. Each stack frame has a name and
/// variables associated with the frame.
#[derive(Debug, Trace, Finalize)]
struct StackFrame {
    label: String,
    variables: VariableMapping,
}

/// The call stack of an interpreter. Standard LIFO implementation.
#[derive(Debug, Default, Trace, Finalize)]
pub struct Stack(Vec<StackFrame>);

struct StackIter<'a> {
    frames: &'a [StackFrame],
}

impl<'a> Iterator for StackIter<'a> {
    type Item = &'a StackFrame;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.frames.last();
        if let Some(frames) = self.frames.get(..self.frames.len() - 2) {
            self.frames = frames;
        } else {
            self.frames = &[];
        }
        next
    }
}

impl Stack {
    /// Create a new, empty call stack.
    pub const fn new() -> Self {
        Self(vec![])
    }

    fn iter(&self) -> StackIter {
        StackIter {
            frames: self.0.as_ref(),
        }
    }

    /// Get the label of the current frame.
    pub fn get_frame_label(&self) -> Result<&String> {
        Ok(&self.0.last().context("nothing in the stack")?.label)
    }

    pub fn get_executing_function_label(&self) -> Option<&String> {
        for frame in self.0.iter().rev() {
            if !SpecialScope::is_label_special_scope(&frame.label) {
                return Some(&frame.label);
            }
        }

        None
    }

    /// Get the variables of the current frame.
    pub fn get_frame_variables(&self) -> Result<&VariableMapping> {
        Ok(&self.0.last().context("nothing in the stack")?.variables)
    }

    /// Extend the call stack by adding a new frame.
    pub fn extend(&mut self, label: String) {
        log::trace!("Stack ++PUSH {label}");
        self.0.push(StackFrame {
            label,
            variables: VariableMapping::default(),
        });
    }

    /// Get the number of stack frames in use.
    pub fn size(&self) -> usize {
        self.0.len()
    }

    /// Pop the top of the stack frame, releasing all of its resources.
    pub fn pop(&mut self) {
        let popped = self.0.pop().expect("pop without stack frame");
        log::trace!("Stack --POP {}", popped.label);
    }

    pub fn pop_until_function(&mut self) {
        let mut c = 1;
        for frame in self.0.iter().rev() {
            if SpecialScope::is_label_special_scope(&frame.label) {
                c += 1;
            } else {
                break;
            }
        }

        let size = self.size();

        let popped = self.0.drain(size - c..);

        log::trace!(
            "{}",
            popped.fold("Stack --POP".to_owned(), |str, frame| str
                + " "
                + frame.label.as_str())
        );
    }

    /// Search the call stack for a frame with a variable with a matching `name`. Will start at the top (most recent)
    /// and will continue until the very first stack frame.
    pub fn find_name(&self, name: &str) -> Option<PrimitiveFlagsPair> {
        for stack_frame in self.0.iter().rev() {
            let tuple = stack_frame.variables.get(name);
            if let Some(packed) = tuple {
                let is_exclusive = { packed.flags().is_exclusive_to_frame() };

                if !is_exclusive {
                    return Some(packed);
                } else {
                    continue;
                }
            }
        }

        None
    }

    pub fn find_name_in_function(&self, name: &str) -> Option<PrimitiveFlagsPair> {
        for stack_frame in self.0.iter().rev() {
            let tuple = stack_frame.variables.get(name);

            if let Some(packed) = tuple {
                let is_exclusive = { packed.flags().is_exclusive_to_frame() };

                if !is_exclusive {
                    return Some(packed);
                } else {
                    continue;
                }
            }

            if !SpecialScope::is_label_special_scope(&stack_frame.label) {
                break;
            }
        }

        None
    }

    /// Add a `name -> variable` mapping to the current stack frame, possibly searching, with default flags.
    pub fn register_variable(&mut self, name: Cow<'static, str>, var: Primitive) -> Result<()> {
        self.register_variable_flags(name, var, VariableFlags::none())
    }

    pub fn ref_variable(&mut self, name: Cow<'static, str>, var: PrimitiveFlagsPair) {
        let stack_frame = self.0.last_mut().expect("nothing in the stack");
        let variables = &mut stack_frame.variables.0;
        variables.insert(name.into_owned(), var);
    }

    /// Add a `name -> variable` mapping to the current stack frame, with flags.
    pub fn register_variable_local(
        &mut self,
        name: String,
        var: Primitive,
        flags: VariableFlags,
    ) -> Result<()> {
        let stack_frame = self.0.last_mut().context("nothing in the stack")?;
        let variables = &mut stack_frame.variables.0;
        variables.insert(name, PrimitiveFlagsPair::new(var, flags));

        Ok(())
    }

    /// Delete a variable from the current stack frame.
    pub fn delete_variable_local(&mut self, name: &str) -> Result<PrimitiveFlagsPair> {
        let frame = self.0.last_mut().expect("no stack frame");

        frame.variables.0.remove(name).ok_or(anyhow!(
            "{name} has not been mapped at this scope, and cannot be deleted (existing: {:?})",
            frame.variables.0
        ))
    }

    /// Add a `name -> variable` mapping to the current stack frame, with special flags.
    pub fn register_variable_flags(
        &mut self,
        name: Cow<'static, str>,
        var: Primitive,
        flags: VariableFlags,
    ) -> Result<()> {
        for frame in self.0.iter().rev() {
            if let Some(ref mapping) = frame.variables.get(&name) {
                if mapping.flags().is_read_only() {
                    bail!("cannot reassign to read-only variable {name}");
                }

                let new_flags_bitfield = flags.0;
                let previos_flags_bitfield = mapping.flags().bits();

                // if a new flag was introduced that the original did not have
                if new_flags_bitfield | previos_flags_bitfield != previos_flags_bitfield {
                    bail!("cannot assign bitfield {new_flags_bitfield:8b} to variable {name}, which has bitfield {previos_flags_bitfield:8b}");
                }

                mapping.set_primitive(var);

                return Ok(());
            }

            if !SpecialScope::is_label_special_scope(&frame.label) {
                break;
            }
        }

        self.register_variable_local(name.into_owned(), var, flags)?;

        Ok(())
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(first) = self.0.last() else {
            return write!(f, "<Empty Stack>");
        };

        write!(f, "\t>> {}", first.label)?; // print the cause first

        for stack_frame in self.0[..self.size() - 1].iter().rev() {
            write!(f, "\r\n\t ^ {}", stack_frame.label)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::Stack;

    #[test]
    fn add_frame() {
        let mut stack = Stack::new();

        let one = String::from("Main");
        let two = String::from("hi");

        stack.extend(one);
        stack.extend(two);
    }
}
