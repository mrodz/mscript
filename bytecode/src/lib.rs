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
