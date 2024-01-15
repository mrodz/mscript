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
    span: String,
}

impl Compile for Assertion {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let mut result = self.value.compile(state)?;

        result.push(instruction!(assert(self.span)));
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
                format!("assert expects a `bool` value, but `{value_ty}` was supplied"),
            )]);
        }

        let input_span = input.as_span();

        let (line, col) = input_span.start_pos().line_col();

        Ok(Assertion {
            value,
            span: format!("{}:{line}:{col}", input.user_data().get_source_file_name()),
        })
    }
}
