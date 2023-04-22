mod attributes_parser;
mod context;
pub mod file;
mod function;
mod instruction;
pub mod interpreter;
mod stack;

use std::sync::Arc;

pub use file::MScriptFile;
pub use stack::Stack;
mod variables;

#[inline(always)]
pub fn arc_to_ref<T>(arc: &Arc<T>) -> &'static mut T {
	unsafe {
		&mut (*(Arc::as_ptr(arc) as *mut T))
	}
}