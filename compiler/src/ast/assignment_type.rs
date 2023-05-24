use std::borrow::Cow;

use anyhow::Result;

use crate::{parser::{Node, Parser}, ast::{Ident, TypeLayout}};

use super::{Assignment, Value, r#type::IntoType};

impl Parser {
    pub fn assignment_type(input: Node) -> Result<Assignment> {
		let mut children = input.children();

		let ident: Node = children.next().unwrap();
		let ty: Node = children.next().unwrap();
		let value: Node = children.next().unwrap();

		let mut ident: Ident = Self::ident(ident);
		let ty: &'static TypeLayout = Self::r#type(ty)?;
		let value: Value = Self::value(value)?;

		ident.link(input.user_data(), Some(Cow::Borrowed(ty)))?;

		let assignment = Assignment { ident, value };

		Ok(assignment)
    }
}