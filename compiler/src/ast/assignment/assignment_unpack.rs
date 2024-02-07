use std::borrow::Cow;

use crate::{
    ast::Ident,
    parser::{Node, Parser, Rule},
    VecErr,
};

use crate::ast::{
    list::ListBound,
    new_err,
    r#type::{IntoType, NativeType},
    Assignment, Number, TypeLayout, Value,
};

impl Parser {
    pub fn assignment_unpack(
        input: Node,
        is_const: bool,
        is_modify: bool,
    ) -> Result<(Assignment, Option<Ident>), Vec<anyhow::Error>> {
        let file_name = &input.user_data().get_file_name();

        let input_span = input.as_span();

        if is_modify {
            return Err(vec![new_err(
                input_span,
                file_name,
                "assignment unpacking does not support the modify keyword".to_owned(),
            )]);
        }

        let children = input.children();

        let mut idents = vec![];
        let mut spans = vec![];

        let mut value_node = None;

        let mut maybe_collision = None;

        for child in children {
            match child.as_rule() {
                Rule::ident => (),
                Rule::value => {
                    value_node = Some(child);
                    break;
                }
                rule => unreachable!("{rule:?}"),
            }

            let ident_span = child.as_span();
            let mut ident = Self::ident(child).to_err_vec()?;

            if is_const {
                ident.mark_const();
            }

            if let (true, collison @ Some(..)) = (
                maybe_collision.is_none(),
                input
                    .user_data()
                    .has_name_been_mapped_in_function(ident.name()),
            ) {
                maybe_collision = collison;
            };

            idents.push(ident);
            spans.push(ident_span);
        }

        let value_node = value_node.expect("should have gotten value");

        let value_span = value_node.as_span();

        let value = Self::value(value_node)?;

        let ty = value.for_type().to_err_vec()?;

        let Some(value_ty) = ty.supports_index() else {
            return Err(vec![new_err(
                value_span,
                file_name,
                format!("cannot unpack a value of `{ty}`, which does not support indexing"),
            )]);
        };

        if value_ty.contains_raw_type(&TypeLayout::Native(NativeType::Int)) {
            return Err(vec![new_err(
                value_span,
                file_name,
                format!(
                    "cannot unpack a value of `{ty}`, which does not support indexing by `int`"
                ),
            )]);
        }

        if let Some(ListBound::Numeric(upper_bound)) = ty.has_index_length_property() {
            if idents.len() - 1 > upper_bound {
                return Err(vec![new_err(
					spans.swap_remove(upper_bound),
					file_name,
					format!(
						"the compiler knows that this value has an upper bound for indexing at `{upper_bound}`, and will not permit unpacking past this variable"
					),
				)]);
            }
        }

        for (idx, (ident, ident_span)) in idents.iter_mut().zip(spans).enumerate() {
            let index_as_number: Number = Number::Integer(idx.to_string());
            let Ok(type_at_idx) = ty.get_output_type_from_index(&Value::Number(index_as_number))
            else {
                return Err(vec![new_err(ident_span, file_name, format!("unpacking index [{idx}] is deemed not safe by the compiler because it cannot guarantee it is a valid index"))]);
            };

            if let Some(list_type) = type_at_idx.is_list() {
                if list_type.must_be_const() && !is_const {
                    return Err(vec![new_err(ident_span, file_name, format!("unpacking index [{idx}] is deemed not safe by the compiler because it has the type `{list_type}`, which in order to be safe must be const"))]);
                }
            }

            let cloned = type_at_idx.clone().into_owned();
            ident
                .link_force_no_inherit(input.user_data(), Cow::Owned(cloned))
                .to_err_vec()?;
        }

        Ok((
            Assignment::new_multi(idents.into_boxed_slice(), value),
            maybe_collision, // for now, this will suffice
        ))
    }
}
