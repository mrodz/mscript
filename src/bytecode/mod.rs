mod attributes_parser;
mod instruction;
pub mod interpreter;
mod number;
mod variable;

pub(crate) use instruction::parse_line;

// pub(crate) use attributes_parser::assert_key_val;
