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

use std::sync::Arc;

pub use interpreter::Program;
mod variables;

#[inline(always)]
pub(crate) fn arc_to_ref<T>(arc: &Arc<T>) -> &'static mut T {
    unsafe { &mut (*(Arc::as_ptr(arc) as *mut T)) }
}
