use std::borrow::Cow;

use anyhow::{Result, anyhow, bail};

use super::r#type::IntoType;
use super::{Assignment, Value};
use crate::ast::{Ident, TypeLayout};
use crate::parser::{Node, Parser};

impl Parser {
    pub fn assignment_type(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let ident: Node = children.next().unwrap();
        let ty: Node = children.next().unwrap();
        let value_node: Node = children.next().unwrap();

        let error = value_node.error("type mismatch");

        let mut ident: Ident = Self::ident(ident);
        let ty: &'static TypeLayout = Self::r#type(ty)?;
        let value: Value = Self::value(value_node)?;

        let value_type = value.into_type()?;

        if ty != &value_type {
            bail!(anyhow!("declaration wanted {ty}, but value is {value_type}")
                .context(error))
        }

        ident.link(input.user_data(), Some(Cow::Borrowed(ty)))?;

        let assignment = Assignment { ident, value };

        Ok(assignment)
    }
}
