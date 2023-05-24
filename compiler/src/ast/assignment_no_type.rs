use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{Assignment, Value, r#type::IntoType};

impl Parser {
    pub fn assignment_no_type(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let (Some(ident), Some(rhs)) = (children.next(), children.next()) else {
			unreachable!()
		};

        let mut ident = Self::ident(ident);

        let value = Self::value(rhs)?;

        let user_data = input.user_data();

        match value {
            Value::Function(ref f) => {
                let inherited = ident.link(user_data, Some(f.clone().consume_for_type()))?;
                if !inherited {
                    unreachable!()
                }
            },
            Value::Ident(..) => {
                ident.link_from_pointed_type_with_lookup(user_data)?;
            }
            Value::Number(ref number) => {
                let ty = number.clone().into_type();
                ident.link(user_data, Some(ty))?;
            }
        }


        Ok(Assignment { ident, value })
    }
}