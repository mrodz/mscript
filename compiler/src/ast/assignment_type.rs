use std::borrow::Cow;

use anyhow::{anyhow, Result};

use super::r#type::IntoType;
use super::{map_err, Assignment, Value};
use crate::ast::{Ident, TypeLayout};
use crate::parser::{Node, Parser};

impl Parser {
    pub fn assignment_type(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let ident: Node = children.next().unwrap();
        let ty: Node = children.next().unwrap();
        let value_node: Node = children.next().unwrap();

        // map_err(, span, file_name, message)
        let value_span = value_node.as_span();

        let mut ident: Ident = Self::ident(ident);
        let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;
        let value: Value = Self::value(value_node)?;

        if let Ok(ref assignment_ty) = value.into_type() {
            if ty.as_ref() != assignment_ty {
                let message = format!("declaration wanted {ty}, but value is {assignment_ty}");
                return map_err(
                    Err(anyhow!("incompatible types")),
                    value_span,
                    &input.user_data().get_source_file_name(),
                    message,
                );
            }
        }

        ident.link(input.user_data(), Some(ty))?;

        let assignment = Assignment { ident, value };

        Ok(assignment)
    }
}
