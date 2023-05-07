#![feature(box_patterns)]
#![allow(dead_code)]

mod attributes_parser;
mod context;
mod file;
mod function;
mod instruction;
mod instruction_constants;
mod interpreter;
mod stack;

pub use interpreter::Program;
pub use variables::Primitive as BytecodePrimitive;
pub use function::ReturnValue as FFIReturnValue;

pub mod compilation_lookups {
    use std::borrow::Cow;

    use crate::instruction_constants::{REPR_TO_BIN, BIN_TO_REPR};
    pub use crate::instruction::split_string;

    pub fn string_instruction_representation_to_byte(string: &str) -> Option<&u8> {
        REPR_TO_BIN.get(string.as_bytes())
    }

    pub fn raw_byte_instruction_to_string_representation(byte: u8) -> Option<Cow<'static, str>> {
        let byte_string = BIN_TO_REPR.get(byte as usize)?;
        let as_str = String::from_utf8_lossy(&byte_string);

        Some(as_str)
    }
}

mod variables;

#[macro_export]
macro_rules! raise_error {
    ($message:tt) => {
        return {
            use std::alloc::{alloc, dealloc, Layout};

            let message = $message.to_string();

            FFIReturnValue::FFIError(message)
        }
    }
    
    //  {{
    //     let message: String = ;
    //     let message: &str = message.as_str();
    //     let (message, length) = (message.as_ptr(), message.len());
    //     return FFIReturnValue::FFIError { message: $message.to_string().as_str().as_ptr(), length }
    // }}
}

#[inline(always)]
pub(crate) fn arc_to_ref<T>(arc: &std::sync::Arc<T>) -> &'static mut T {
    unsafe { &mut (*(std::sync::Arc::as_ptr(arc) as *mut T)) }
}
