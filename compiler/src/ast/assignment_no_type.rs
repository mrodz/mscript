use std::borrow::Cow;

use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{map_err_messages, r#type::IntoType, Assignment, Value};

impl Parser {
    pub fn assignment_no_type(input: Node, is_const: bool) -> Result<(Assignment, bool)> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let rhs = children.next().unwrap();

        let mut ident = Self::ident(ident)?;

        let did_exist_before = input.user_data().has_name_been_mapped_local(ident.name());
        
        if is_const {
            ident.mark_const();
        }

        let value = Self::value(rhs)?;

        let user_data = input.user_data();

        let maybe_error: Result<()> = try {
            match value {
                Value::Function(ref f) => {
                    ident.link_force_no_inherit(user_data, Cow::Owned(f.for_type()?))?;
                }
                Value::Ident(..) => {
                    ident.link_from_pointed_type_with_lookup(user_data)?;
                }
                Value::Number(ref number) => {
                    let ty = number.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::String(ref string) => {
                    let ty = string.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::MathExpr(ref math_expr) => {
                    let ty = math_expr.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::Callable(ref callback) => {
                    let ty = callback.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
                Value::Boolean(ref boolean) => {
                    let ty = boolean.for_type()?;
                    ident.link_force_no_inherit(user_data, Cow::Owned(ty))?;
                }
            }
        };

        map_err_messages(
            maybe_error,
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "could not get the type".into(),
            || vec!["could not understand the type of the input"],
        )?;

        Ok((Assignment::new(ident, value), did_exist_before))
    }
}
