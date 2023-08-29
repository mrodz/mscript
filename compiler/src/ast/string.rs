use std::borrow::Cow;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{
    r#type::{IntoType, NativeType, StrWrapper},
    Compile, CompiledItem, Dependencies, Dependency, TypeLayout, CompilationState,
};

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum AstString {
    Plain(String),
    FormattedString(),
}

impl Dependencies for AstString {
    fn dependencies(&self) -> Vec<Dependency> {
        match self {
            AstString::Plain(_) => vec![],
            AstString::FormattedString() => todo!(),
        }
    }
}

impl IntoType for AstString {
    fn for_type(&self) -> Result<super::TypeLayout> {
        match self {
            AstString::Plain(str) => Ok(TypeLayout::Native(NativeType::Str(StrWrapper(Some(
                str.len(),
            ))))),
            AstString::FormattedString() => todo!(),
        }
    }
}

impl Compile for AstString {
    fn compile(&self, _: &mut CompilationState) -> Result<Vec<CompiledItem>> {
        match self {
            AstString::Plain(content) => Ok(vec![instruction!(string content)]),
            AstString::FormattedString() => todo!(),
        }
    }
}

impl Parser {
    pub fn string(input: Node) -> Result<AstString> {
        let as_str = input.as_str();
        let encoded = bytecode::compilation_bridge::split_string(Cow::Borrowed(as_str))?[0].clone();

        Ok(AstString::Plain(encoded))
    }
}
