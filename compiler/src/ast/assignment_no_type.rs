use std::borrow::Cow;

use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{map_err_messages, r#type::IntoType, Assignment, Value};

impl Parser {
    pub fn assignment_no_type(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let rhs = children.next().unwrap();

        let mut ident = Self::ident(ident)?;
        let value = Self::value(rhs)?;

        let user_data = input.user_data();

        let maybe_error: Result<()> = try {
            match value {
                Value::Function(ref f) => {
                    ident.link(user_data, Some(Cow::Owned(f.clone().consume_for_type()?)))?;
                }
                Value::Ident(..) => {
                    ident.link_from_pointed_type_with_lookup(user_data)?;
                }
                Value::Number(ref number) => {
                    let ty = number.clone().for_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::String(ref string) => {
                    let ty = string.for_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::MathExpr(ref math_expr) => {
                    let ty = math_expr.for_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::Callable(ref callback) => {
                    let ty = callback.for_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::Boolean(ref boolean) => {
                    let ty = boolean.for_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
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

        Ok(Assignment { ident, value })
    }
}
