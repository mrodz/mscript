use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{r#type::IntoType, Compile, Dependencies};

impl Compile for bool {
    fn compile(
        &self,
        _: &mut Vec<super::CompiledItem>,
    ) -> anyhow::Result<Vec<super::CompiledItem>> {
        Ok(vec![instruction!(bool self)])
    }
}

impl Dependencies for bool {}

impl IntoType for bool {
    fn for_type(&self) -> anyhow::Result<super::TypeLayout> {
        Ok(super::TypeLayout::Native(super::r#type::NativeType::Bool))
    }
}

pub fn boolean_from_str(str: &str) -> bool {
    str == "true"
}

impl Parser {
    #[allow(dead_code)]
    pub fn boolean(input: Node) -> bool {
        boolean_from_str(input.as_str())
    }
}
