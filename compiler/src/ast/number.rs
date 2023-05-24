use std::fmt::Display;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{
    r#type::{IntoType, NativeType},
    Compile, Dependencies, TypeLayout,
};

#[derive(Debug, Clone)]
pub enum Number {
    Integer(String),
    BigInt(String),
    Float(String),
    Byte(String),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Number::*;
        let str = match self {
            Integer(str) | Float(str) | Byte(str) | BigInt(str) => str.clone(),
        };

        write!(f, "{str}")
    }
}

impl Dependencies for Number {}

impl IntoType for Number {
    fn into_type(&self) -> super::TypeLayout {
        match self {
            Self::Byte(_) => TypeLayout::Native(NativeType::Byte),
            Self::Integer(_) => TypeLayout::Native(NativeType::Int),
            Self::BigInt(_) => TypeLayout::Native(NativeType::BigInt),
            Self::Float(_) => TypeLayout::Native(NativeType::Float),
        }
    }
}

impl Compile for Number {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let matched = match self {
            Number::Byte(val) => vec![instruction!(byte val)],
            Number::Float(val) => vec![instruction!(float val)],
            Number::Integer(val) => vec![instruction!(int val)],
            Number::BigInt(val) => vec![instruction!(bigint val)]
        };

        Ok(matched)
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