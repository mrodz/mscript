use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
    VecErr,
};

use super::{
    new_err, r#type::IntoType, CompilationState, Compile, CompiledItem, Dependencies, Value,
};

#[derive(Debug)]
pub struct Assertion {
    value: Value,
    start: usize,
    end: usize,
}

impl Compile for Assertion {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let mut result = self.value.compile(state)?;

        let start = self.start;
        let end = self.end;

        result.push(instruction!(assert start end));
        Ok(result)
    }
}

impl Dependencies for Assertion {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.value.net_dependencies()
    }
}

impl Parser {
    pub fn assertion(input: Node) -> Result<Assertion, Vec<anyhow::Error>> {
        let value_node = input.children().single().unwrap();
        let value_span = value_node.as_span();
        let value = Self::value(value_node)?;

        let value_ty = value.for_type().to_err_vec()?;
        if !value_ty.is_boolean() {
            return Err(vec![new_err(
                value_span,
                &input.user_data().get_file_name(),
                format!("assert expects boolean, but `{value_ty}` was supplied"),
            )]);
        }

        let input_span = input.as_span();

        Ok(Assertion {
            value,
            start: input_span.start(),
            end: input_span.end(),
        })
    }
}
