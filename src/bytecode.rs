mod attributes_parser;
pub mod file;
mod function;
mod instruction;
pub mod interpreter;
mod stack;
mod context;

pub use file::MScriptFile;
pub use stack::Stack;
mod variables;
