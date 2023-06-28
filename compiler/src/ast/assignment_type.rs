use std::borrow::Cow;

use anyhow::{Result, bail};

use super::r#type::IntoType;
use super::{Assignment, Value, new_err};
use crate::ast::{Ident, TypeLayout};
use crate::parser::{Node, Parser};

impl Parser {
    pub fn assignment_type(input: Node, is_const: bool) -> Result<(Assignment, bool)> {
        let mut children = input.children();

        let ident: Node = children.next().unwrap();
        let ty: Node = children.next().unwrap();
        let value: Node = children.next().unwrap();

        let ty_span = ty.as_span();

        let mut ident: Ident = Self::ident(ident)?;

        let did_exist_before = input.user_data().has_name_been_mapped_local(ident.name());

        if is_const {
            ident.mark_const();
        }

        let ty: Cow<'static, TypeLayout> = Self::r#type(ty)?;
        let value: Value = Self::value(value)?;

        if let Ok(ref assignment_ty) = value.for_type() {
            if ty.as_ref() != assignment_ty {
                let message = if value.is_callable() {
                    format!("declaration wanted {ty}, but value is a function that returns {assignment_ty}")
                } else {
                    // TODO: special check for function types.
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

        Ok((assignment, did_exist_before))
    }
}
