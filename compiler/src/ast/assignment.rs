use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{value::Value, Ident};

#[derive(Debug)]
pub struct Assignment {
    ident: Ident,
    value: Value,
}

impl Parser {
    pub fn assignment(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let (Some(ident), Some(rhs)) = (children.next(), children.next()) else {
			unreachable!()
		};

        let ident = Self::ident(ident);

        let value = Self::value(rhs)?;

        Ok(Assignment { ident, value })
    }
}
