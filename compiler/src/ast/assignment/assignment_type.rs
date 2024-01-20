use anyhow::Result;

use crate::ast::r#type::TypecheckFlags;
use crate::ast::ClassType;
use crate::ast::{new_err, r#type::IntoType, Assignment, Ident, Value};
use crate::parser::{Node, Parser};
use crate::VecErr;

impl Parser {
    pub fn assignment_type(
        input: Node,
        is_const: bool,
        is_modify: bool,
        self_type: Option<ClassType>,
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

        let ty = Self::r#type(ty).to_err_vec()?;
        let value: Value = Self::value(value)?;

        if let Ok(ref assignment_ty) = value.for_type() {
            if !ty.as_ref().get_type_recursively().eq_complex(
                assignment_ty.get_type_recursively(),
                &TypecheckFlags::use_class(self_type.as_ref()).lhs_unwrap(false),
            ) {
                let hint = ty
                    .get_error_hint_between_types(
                        assignment_ty,
                        input.user_data().get_type_of_executing_class(),
                    )
                    .unwrap_or_default();

                let message = if let Some(as_callable) = assignment_ty.is_callable() {
                    format!("declaration wanted `{ty}`, but value is a function that returns `{}`{hint}", as_callable.return_type())
                } else {
                    // TODO: special check for function types.
                    format!("declaration wanted `{ty}`, but value is `{assignment_ty}`{hint}")
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
