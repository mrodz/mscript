use std::{borrow::Cow, fmt::Debug, slice::Iter};

use crate::{
    parser::{Node, Parser},
    VecErr,
};

use super::{
    new_err,
    r#type::{IntoType, TypecheckFlags},
    Dependencies, Dependency, FunctionParameters, TypeLayout, Value,
};

#[derive(Debug)]
pub(crate) struct FunctionArguments(Vec<Value>);

impl FunctionArguments {
    pub fn iter(&self) -> Iter<Value> {
        self.0.iter()
    }
}

impl Dependencies for FunctionArguments {
    fn dependencies(&self) -> Vec<Dependency> {
        self.0.iter().flat_map(|x| x.net_dependencies()).collect()
    }
}

impl Parser {
    pub fn function_arguments(
        input: Node,
        expected_parameters: &FunctionParameters,
        allow_self_type: Option<&TypeLayout>,
    ) -> Result<FunctionArguments, Vec<anyhow::Error>> {
        let children = input.children();

        let mut result = vec![];
        let mut result_len: usize = 0;
        let mut errors = vec![];

        let expected_types: Cow<Vec<Cow<TypeLayout>>> = expected_parameters.to_types();

        let mut child_span = input.as_span();

        for (idx, child) in children.enumerate() {
            if idx == expected_types.len() {
                if idx == 0 {
                    return Err(vec![new_err(
                        child_span,
                        &input.user_data().get_source_file_name(),
                        "this function specifies zero parameters, but found > 0 arguments"
                            .to_owned(),
                    )]);
                }
                break;
            }

            child_span = child.as_span();
            let value_for_arg = Self::value(child)?;

            let arg_ty = value_for_arg.for_type().to_err_vec()?;

            let expected_ty_at_idx = &expected_types[idx];

            let user_gave = arg_ty.get_type_recursively();

            let maybe_class_type = allow_self_type.and_then(|x| {
                let TypeLayout::Class(class_type) = x.disregard_distractors(true) else {
                    return None;
                };

                Some(class_type)
            });

            result_len += 1;

            let active_class = input.user_data().get_type_of_executing_class();

            if let Some(ref c) = active_class {
                println!("\tFOR {c}");
            }
            let class_sent_for_comparison = maybe_class_type.or(active_class.as_deref());

            println!("\t- {result_len} {user_gave} & {expected_ty_at_idx} with {class_sent_for_comparison:?}");

            if !expected_ty_at_idx.eq_complex(
                user_gave,
                &TypecheckFlags::use_class(class_sent_for_comparison).lhs_unwrap(false),
            ) {
                let argument_number = idx + 1;
                let hint = expected_ty_at_idx
                    .get_error_hint_between_types(user_gave, active_class)
                    .unwrap_or_default();
                let error_message = format!("type mismatch when calling function (argument #{argument_number} was expected to be `{expected_ty_at_idx}` based on type signature, instead found `{user_gave}`){hint}", );
                errors.push(new_err(
                    child_span,
                    &input.user_data().get_source_file_name(),
                    error_message,
                ));

                continue;
            }

            result.push(value_for_arg)
        }

        let expected_parameters_len = expected_parameters.len();

        if result_len != expected_parameters_len {
            let result_plural = if result_len == 1 { "" } else { "s" };

            let expected_plural = if expected_parameters_len == 1 {
                ""
            } else {
                "s"
            };

            let msg = format!("supplied {result_len} argument{result_plural}, but this function's signature specifies {expected_parameters_len} parameter{expected_plural} (Expected arguments: `({expected_parameters})`)");

            errors.push(new_err(
                child_span,
                &input.user_data().get_source_file_name(),
                msg,
            ))
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(FunctionArguments(result))
    }
}
