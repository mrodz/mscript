use std::{borrow::Cow, slice::Iter};

use crate::{
    parser::{Node, Parser},
    VecErr,
};

use super::{
    new_err, r#type::IntoType, value::ValueChain, Dependencies, Dependency,
    FunctionParameters, TypeLayout,
};

#[derive(Debug)]
pub(crate) struct FunctionArguments(Vec<ValueChain>);

impl FunctionArguments {
    pub fn iter(&self) -> Iter<ValueChain> {
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
    ) -> Result<FunctionArguments, Vec<anyhow::Error>> {
        let children = input.children();

        let mut result = vec![];
        let mut errors = vec![];

        let expected_types: Cow<Vec<Cow<TypeLayout>>> = expected_parameters.to_types();

        for (idx, child) in children.enumerate() {
            let child_span = child.as_span();
            let value_for_arg = Self::value(child)?;

            let arg_ty = value_for_arg.for_type().to_err_vec()?;

            let expected_ty_at_idx = &expected_types[idx];

            let user_gave = arg_ty.get_type_recursively();


            if !user_gave.eq(expected_ty_at_idx) {
                dbg!(user_gave, expected_ty_at_idx);

                let argument_number = idx + 1;
                let error_message = format!("type mismatch when calling function (argument #{argument_number} was expected to be `{expected_ty_at_idx}` based on type signature, instead found `{user_gave}`)");
                errors.push(new_err(
                    child_span,
                    &input.user_data().get_source_file_name(),
                    error_message,
                ));
                
                continue;
            }

            result.push(value_for_arg)
        }

        if !errors.is_empty() {
            return Err(errors)
        }

        Ok(FunctionArguments(result))
    }
}
