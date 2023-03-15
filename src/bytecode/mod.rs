mod attributes_parser;
mod function;
mod instruction;
pub mod file;
pub mod interpreter;
mod stack;

pub use stack::Stack;
pub use file::MScriptFile;

// struct JumpRequest {
//     destination: Arc<File>
// }

// pub trait Jumpable {
// 	fn jump() -> 
// }
mod variable;
