use anyhow::Result;

use crate::parser::{Node, Parser, Rule};

use super::Dependencies;

#[derive(Debug)]
pub enum Number {
    Integer(String),
    Float(String),
    Byte(String),
}

impl Dependencies for Number {}

impl Parser {
    pub fn number(input: Node) -> Result<Number> {
        let child = input.children().next().unwrap();

        let as_str = child.as_str().chars().filter(|x| x != &'_').collect();

        let matched = match child.as_rule() {
            Rule::integer => Number::Integer(as_str),
			Rule::hex_int => {
				let as_hex = i128::from_str_radix(&as_str[2..], 16)
					.map_err(|e| child.error(e))?
					.to_string();

				Number::Integer(as_hex)
			},
            Rule::float => Number::Float(as_str),
            Rule::byte => Number::Byte(as_str),
            _ => unreachable!(),
        };

		Ok(matched)
    }
}
