use std::borrow::Cow;

use anyhow::Result;

use crate::ast::{new_err, r#type::IntoType, Assignment, Ident, TypeLayout, Value};
use crate::parser::{Node, Parser};
use crate::VecErr;

impl Parser {
    pub fn assignment_type(
        input: Node,
        is_const: bool,
        is_modify: bool,
    ) -> Result<(Assignment, bool), Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident: Node = children.next().unwrap();
        let ty: Node = children.next().unwrap();
        let value: Node = children.next().unwrap();

        let ty_span = ty.as_span();

        let mut ident: Ident = Self::ident(ident).to_err_vec()?;

        let did_exist_before = input.user_data().has_name_been_mapped_local(ident.name());

        if is_const {
            ident.mark_const();
        }

        let ty: Cow<'static, TypeLayout> = Self::r#type(ty).to_err_vec()?;
        let value: Value = Self::value(value)?;

        if let Ok(ref assignment_ty) = value.for_type() {
            if ty.as_ref() != assignment_ty {
                let hint = ty.get_error_hint_between_types(assignment_ty).map_or_else(
                    || Cow::Borrowed(""),
                    |str| Cow::Owned(format!("\n\t- hint: {str}")),
                );

                let message = if value.is_callable().to_err_vec()? {
                    format!("declaration wanted {ty}, but value is a function that returns {assignment_ty}{hint}")
                } else {
                    // TODO: special check for function types.
                    format!("declaration wanted {ty}, but value is {assignment_ty}{hint}")
                };

                return Err(vec![new_err(
                    ty_span,
                    &input.user_data().get_source_file_name(),
                    message,
                )]);
            }
        }

        ident
            .link_force_no_inherit(input.user_data(), ty)
            .to_err_vec()?;

        if is_modify {
            ident = ident.wrap_in_callback().to_err_vec()?;
        }

        let assignment = Assignment::new(ident, value);

        Ok((assignment, did_exist_before))
    }
}
