use anyhow::Result;

use crate::{
    ast::Ident,
    parser::{Node, Parser},
    VecErr,
};

use crate::ast::{map_err_messages, Assignment};

impl Parser {
    pub fn assignment_no_type(
        input: Node,
        is_const: bool,
        is_modify: bool,
    ) -> Result<(Assignment, Option<Ident>), Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let rhs = children.next().unwrap();

        let mut ident = Self::ident(ident).to_err_vec()?;

        let did_exist_before = if !is_modify {
            input
                .user_data()
                .has_name_been_mapped_in_function(ident.name())
        } else {
            input
                .user_data()
                .get_dependency_flags_from_name(ident.name())
                .map(|x| x.0.to_owned())
        };

        if is_const {
            ident.mark_const();
        }

        let value = Self::value(rhs)?;

        let user_data = input.user_data();

        let maybe_error = value.associate_with_ident(&mut ident, user_data);

        if is_modify {
            ident = ident.wrap_in_callback().to_err_vec()?;
        }

        map_err_messages(
            maybe_error,
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "could not get the type".into(),
            || vec!["Could not infer the type of the value; try explicitly defining a type"],
        )
        .to_err_vec()?;

        Ok((Assignment::new(ident, value), did_exist_before))
    }
}
