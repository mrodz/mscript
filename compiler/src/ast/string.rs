use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{
    r#type::{IntoType, NativeType},
    Compile, Dependencies, TypeLayout,
};

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum AstString {
    Plain(String),
    FormattedString(),
}

impl Dependencies for AstString {
    fn get_dependencies(&self) -> Option<Box<[super::Dependency]>> {
        match self {
            AstString::Plain(_) => None,
            AstString::FormattedString() => todo!(),
        }
    }
}

impl IntoType for AstString {
    fn into_type(&self) -> Result<super::TypeLayout> {
        Ok(TypeLayout::Native(NativeType::Str))
    }
}

impl Compile for AstString {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        match self {
            AstString::Plain(content) => Ok(vec![instruction!(string content)]),
            AstString::FormattedString() => todo!(),
        }
    }
}

impl Parser {
    pub fn string(input: Node) -> Result<AstString> {
        let as_str = input.as_str();
        // let len = as_str.len();

        // assert!(len >= 3);

        // let as_str = as_str
		// 	.get(1..len - 1)
		// 	.unwrap()
		// 	.to_owned();

        Ok(AstString::Plain(as_str.to_string()))
    }
}
