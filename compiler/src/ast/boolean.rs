use crate::instruction;

use super::{r#type::IntoType, CompilationState, Compile, Dependencies};

impl Compile for bool {
    fn compile(&self, _: &CompilationState) -> anyhow::Result<Vec<super::CompiledItem>> {
        Ok(vec![instruction!(bool self)])
    }
}

impl Dependencies for bool {}

impl IntoType for bool {
    fn for_type(&self) -> anyhow::Result<super::TypeLayout> {
        Ok(super::TypeLayout::Native(super::r#type::NativeType::Bool))
    }
}
