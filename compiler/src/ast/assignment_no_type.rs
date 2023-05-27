use std::borrow::Cow;

use anyhow::{Result, anyhow, Context};

use crate::parser::{Node, Parser};

use super::{r#type::IntoType, Assignment, Value};

impl Parser {
    pub fn assignment_no_type(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let rhs = children.next().unwrap();

        let mut ident = Self::ident(ident);
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
                    let ty = number.clone().into_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::String(ref string) => {
                    let ty = string.into_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
                Value::MathExpr(ref math_expr) => {
                    let ty = math_expr.into_type()?;
                    ident.link(user_data, Some(Cow::Owned(ty)))?;
                }
            }
        };

        maybe_error
            .context("could not understand the type of the input")
            .context(anyhow!(input.error("could not get type")))?;

        Ok(Assignment { ident, value })
    }
}
