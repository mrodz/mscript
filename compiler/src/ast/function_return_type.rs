use std::borrow::Cow;

use anyhow::Result;

use crate::parser::{Node, Parser};

use super::TypeLayout;

impl Parser {
    pub fn function_return_type(input: Node) -> Result<Cow<'static, TypeLayout>> {
        let ty = input.children().next().unwrap();
        Self::r#type(ty)
    }
}
