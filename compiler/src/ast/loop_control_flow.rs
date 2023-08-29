use anyhow::Result;

use crate::{
    ast::map_err,
    parser::{Node, Parser},
};

use super::{Compile, CompiledItem};

#[derive(Debug)]
pub(crate) struct Continue {
    frames_since_loop: usize,
}

#[derive(Debug)]
pub(crate) struct Break {
    frames_since_loop: usize,
}

impl Compile for Continue {
    fn compile(
        &self,
        _function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>, anyhow::Error> {
        Ok(vec![CompiledItem::Continue(self.frames_since_loop)])
    }
}

impl Compile for Break {
    fn compile(
        &self,
        _function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>, anyhow::Error> {
        Ok(vec![CompiledItem::Break(self.frames_since_loop)])
    }
}

impl Parser {
    pub fn break_statement(input: Node) -> Result<Break> {
        // TODO: check if is in loop
        let frames_since_loop = input.user_data().scopes_since_loop();

        let frames_since_loop = map_err(
            frames_since_loop,
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "cannot use `break` outside of a loop".to_owned(),
        )?;

        Ok(Break { frames_since_loop })
    }

    pub fn continue_statement(input: Node) -> Result<Continue> {
        // TODO: check if is in loop
        let frames_since_loop = input.user_data().scopes_since_loop();

        let frames_since_loop = map_err(
            frames_since_loop,
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "cannot use `continue` outside of a loop".to_owned(),
        )?;

        Ok(Continue { frames_since_loop })
    }
}
