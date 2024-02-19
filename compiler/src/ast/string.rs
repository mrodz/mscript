use anyhow::Result;
use bytecode::compilation_bridge::split_string_v2;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{
    r#type::{IntoType, NativeType, StrWrapper},
    CompilationState, Compile, CompiledItem, Dependencies, Dependency, TypeLayout,
};

#[derive(Debug, Clone)]
pub enum AstString {
    Plain(String),
    #[allow(unused)]
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
    fn for_type(&self) -> Result<TypeLayout> {
        match self {
            AstString::Plain(str) => Ok(TypeLayout::Native(NativeType::Str(StrWrapper(Some(
                str.len(),
            ))))),
            AstString::FormattedString() => todo!(),
        }
    }
}

impl Compile for AstString {
    fn compile(&self, _: &CompilationState) -> Result<Vec<CompiledItem>> {
        match self {
            AstString::Plain(content) => Ok(vec![instruction!(make_str content)]),
            AstString::FormattedString() => todo!(),
        }
    }
}

impl Parser {
    pub fn string(input: Node) -> Result<AstString> {
        let as_str = input.as_str();
        let bridge_parts = split_string_v2(&("\"".to_owned() + as_str + "\""), false)?;

        let encoded = bridge_parts[0].clone();

        Ok(AstString::Plain(encoded))
    }
}
