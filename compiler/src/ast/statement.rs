use crate::parser::{Parser, Node};

#[derive(Debug)]
pub enum Statement {
	HelloStatement
}

impl Parser {
	pub fn statement(_input: Node) -> Statement {
		Statement::HelloStatement
	}
}