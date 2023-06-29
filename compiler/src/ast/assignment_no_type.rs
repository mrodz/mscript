use anyhow::Result;

use crate::parser::{Node, Parser};

use super::{map_err_messages, Assignment};

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

        let maybe_error = value.associate_with_ident(&mut ident, user_data);
        
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
