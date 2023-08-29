//! This is the API for all-things-bytecode.

#![allow(dead_code)]

pub(crate) mod context;
pub(crate) mod file;
pub(crate) mod function;
pub(crate) mod instruction;
pub(crate) mod instruction_constants;
pub(crate) mod interpreter;
pub(crate) mod stack;

mod variables;

// Alternate naming to make writing FFI functions simpler.
pub use function::ReturnValue as FFIReturnValue;
pub use variables::Primitive as BytecodePrimitive;

// Expose the Program interface.
pub use interpreter::Program;

/// Useful functions that make `bytecode -> ir`, `mscript -> bytecode`, and `bytecode -> ir` conversions easier.
pub mod compilation_bridge {
    use crate::instruction_constants::{BIN_TO_REPR, REPR_TO_BIN};
    use std::borrow::Cow;

    pub use crate::instruction::split_string;

    pub use crate::file::{MScriptFile, MScriptFileBuilder};
    pub use crate::instruction::Instruction;

    /// From a string, get an instruction's corresponding byte.
    ///
    /// ```
    /// use bytecode::compilation_bridge::string_instruction_representation_to_byte;
    ///
    /// let printn = string_instruction_representation_to_byte("printn").unwrap();
    /// let if_stmt = string_instruction_representation_to_byte("if").unwrap();
    /// let update = string_instruction_representation_to_byte("update").unwrap();
    ///
    /// assert_eq!(printn, &0x13);
    /// assert_eq!(if_stmt, &0x1C);
    /// assert_eq!(update, &0x28);
    /// ```
    pub fn string_instruction_representation_to_byte(string: &str) -> Option<&u8> {
        REPR_TO_BIN.get(string.as_bytes())
    }

    /// Reverse an instruction from its byte to its name.
    /// ```
    /// use bytecode::compilation_bridge::raw_byte_instruction_to_string_representation;
    /// use std::borrow::Cow;
    ///
    /// let printn = raw_byte_instruction_to_string_representation(0x13).unwrap();
    /// let if_stmt = raw_byte_instruction_to_string_representation(0x1C).unwrap();
    /// let update = raw_byte_instruction_to_string_representation(0x28).unwrap();
    ///
    /// assert_eq!(printn, Cow::Borrowed("printn"));
    /// assert_eq!(if_stmt, Cow::Borrowed("if"));
    /// assert_eq!(update, Cow::Borrowed("update"));
    /// ```
    pub fn raw_byte_instruction_to_string_representation(byte: u8) -> Option<Cow<'static, str>> {
        let byte_string = BIN_TO_REPR.get(byte as usize)?;
        let as_str = String::from_utf8_lossy(byte_string);

        Some(as_str)
    }
}

/// Macro for Rust FFI that is the same as raising an error for the interpreter.
#[macro_export]
macro_rules! raise_error {
    ($message:tt) => {
        return {
            let message = $message.to_string();

            FFIReturnValue::FFIError(message)
        }
    };
}

/// Hacky function to get the contents of an Arc as a mutable reference.
///
/// Red flags galore! Using this function bypasses the safety of using shared references to
/// prevent data races. However, since the current design of the interpreter is single threaded,
/// there is no concern regarding multiple processes attempting to modify the
/// call stack, change a file's flags, etc.
#[inline(always)]
pub(crate) fn rc_to_ref<T>(arc: &std::rc::Rc<T>) -> &'static mut T {
    unsafe { &mut (*(std::rc::Rc::as_ptr(arc) as *mut T)) }
}
