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
        let value: Node = children.next().unwrap();

        // map_err(, span, file_name, message)
        let ty_span = ty.as_span();

        let mut ident: Ident = Self::ident(ident);
        let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;
        let value: Value = Self::value(value)?;

        // dbg!(value_span);

        if let Ok(ref assignment_ty) = value.into_type() {
            if ty.as_ref() != assignment_ty {
                let message = if value.is_callable() {
                    // let return_type = function_type.get_return_type().map_or_else(|| Cow::Borrowed("None"), |ok| Cow::Owned(ok.to_string()));
                    format!("declaration wanted {ty}, but value is a function that returns {assignment_ty}")
                } else {
                    // panic!(" ?????? ");
                    format!("declaration wanted {ty}, but value is {assignment_ty}")
                };

                return map_err(
                    Err(anyhow!("incompatible types")),
                    ty_span,
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
