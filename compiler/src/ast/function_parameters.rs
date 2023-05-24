use anyhow::Result;

use crate::{parser::{Parser, Node}, instruction};

use super::{Ident, Dependencies, Compile, r#type::TypeLayout};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParameters(Vec<(Ident, &'static TypeLayout)>);

impl Dependencies for FunctionParameters {}

impl Compile for FunctionParameters {
	fn compile(&self) -> Result<Vec<super::CompiledItem>> {
		let mut result = vec![];
		for (idx, (ident, ty)) in self.0.iter().enumerate() {
			result.push(instruction!(arg idx));
			result.push(instruction!(store ident));
		}
		Ok(result)
	}
}

impl Parser {
	pub fn function_parameters(input: Node) -> Result<FunctionParameters> {
		let mut children = input.children();

		let mut result = vec![];
		// let user_data = input.user_data();

		while let (Some(ident), Some(ty)) = (children.next(), children.next()) {
			let ident = Self::ident(ident);
			let ty: &'static TypeLayout = Self::r#type(ty)?;

			result.push((ident, ty));
		}

		Ok(FunctionParameters(result))
	}
}