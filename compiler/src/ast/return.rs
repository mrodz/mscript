use std::borrow::Cow;

use anyhow::Result;

use crate::{
    ast::{new_err, r#type::IntoType},
    instruction,
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{Compile, Dependencies, Value};

#[derive(Debug)]
pub(crate) struct ReturnStatement(Option<Value>);

impl Compile for ReturnStatement {
    fn compile(
        &self,
        function_buffer: &mut Vec<super::CompiledItem>,
    ) -> Result<Vec<super::CompiledItem>> {
        let Some(ref return_value) = self.0 else {
			return Ok(vec![instruction!(ret)])
		};

        let mut value_init = return_value.compile(function_buffer)?;
        value_init.push(instruction!(ret));
        Ok(value_init)
    }
}

impl Dependencies for ReturnStatement {
    fn dependencies(&self) -> Vec<super::Dependency> {
        if let Some(ref return_value) = self.0 {
            return_value.net_dependencies()
        } else {
            vec![]
        }
    }
}

impl Parser {
    pub fn return_statement(input: Node) -> Result<ReturnStatement, Vec<anyhow::Error>> {
        let expected_return_type = input.user_data().get_return_type();

        expected_return_type
            .mark_should_return_as_completed()
            .to_err_vec()?;

        let value_node = input.children().next();

        /*
        | WANTS | GETS | !! = error
        | ----- | ---- |
        |   Y   |   N  | !!< #0 Type in signature, blank "return"
        |   Y   |  y/N | !!< #1 Type in signature, type mismatch
        |   Y   |   Y  | <<< #2 Matching type in signature
        |   N   |   Y  | !!< #3 Void function, returns value
        |   N   |   N  | <<< #4 Void function, returns void
        | ----- | ---- |
        */

        let Some(value_node) = value_node else {
			if let ScopeReturnStatus::Did(expected_return_type) = expected_return_type {
				// #0
				return Err(vec![new_err(
					input.as_span(),
					&input.user_data().get_source_file_name(),
					format!(
						"this function was expected to return {expected_return_type}, but no value was supplied"
					)
				)])
			} else {
				// #4
				return Ok(ReturnStatement(None))
			}
		};

        let value = Self::value(value_node)?;

        let supplied_type = value.for_type().to_err_vec()?;
        let supplied_type = supplied_type.get_type_recursively();

        let ScopeReturnStatus::Did(expected_return_type) = expected_return_type else {
			// #3
			return Err(vec![new_err(
				input.as_span(),
				&input.user_data().get_source_file_name(),
				format!(
					"this function did not expect to return a value, but {supplied_type} was supplied"
				)
			)])
		};

        if expected_return_type != &Cow::Borrowed(supplied_type) {
			return Err(vec![new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                format!(
					"this function was expected to return {expected_return_type}, but {supplied_type} was supplied"
				)
            )])
        }

        // #2
        Ok(ReturnStatement(Some(value)))
    }
}
