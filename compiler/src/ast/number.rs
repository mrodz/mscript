use std::fmt::Display;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{Compile, Dependencies};

#[derive(Debug)]
pub enum Number {
    Integer(String),
    Float(String),
    Byte(String),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Number::Integer(str) | Number::Float(str) | Number::Byte(str) => str.clone(),
        };

        write!(f, "{str}")
    }
}

impl Dependencies for Number {}

impl Compile for Number {
    fn compile(&self) -> Vec<super::CompiledItem> {
        match self {
            Number::Byte(val) => vec![instruction!(byte val)],
            Number::Float(val) => vec![instruction!(float val)],
            Number::Integer(val) => vec![instruction!(int val)],
        }
    }
}

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
            }
            Rule::float => Number::Float(as_str),
            Rule::byte => Number::Byte(as_str),
            _ => unreachable!(),
        };

        Ok(matched)
    }
}
