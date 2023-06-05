use std::borrow::Cow;

use anyhow::{Result, bail};

use super::r#type::IntoType;
use super::{Assignment, Value, new_err};
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

        let mut ident: Ident = Self::ident(ident)?;
        let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;
        let value: Value = Self::value(value)?;

        // dbg!(value_span);

        if let Ok(ref assignment_ty) = value.for_type() {
            if ty.as_ref() != assignment_ty {
                let message = if value.is_callable() {
                    // let return_type = function_type.get_return_type().map_or_else(|| Cow::Borrowed("None"), |ok| Cow::Owned(ok.to_string()));
                    format!("declaration wanted {ty}, but value is a function that returns {assignment_ty}")
                } else {
                    format!("declaration wanted {ty}, but value is {assignment_ty}")
                };

                bail!(new_err(
                    ty_span,
                    &input.user_data().get_source_file_name(),
                    message,
                ));
            }
        }

        ident.link_force_no_inherit(input.user_data(), ty)?;

        let assignment = Assignment::new(ident, value);

        Ok(assignment)
    }
}
