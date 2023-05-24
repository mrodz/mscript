use std::borrow::Cow;

use anyhow::Result;

use crate::{parser::{Parser, Node}, instruction};

use super::{Ident, Dependencies, Compile, r#type::TypeLayout};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParameters(Vec<Ident>);

impl Dependencies for FunctionParameters {}

impl Compile for FunctionParameters {
	fn compile(&self) -> Result<Vec<super::CompiledItem>> {
		let mut result = vec![];
		for (idx, ident) in self.0.iter().enumerate() {
			let name = ident.name();
			result.push(instruction!(arg idx));
			result.push(instruction!(store name));
		}
		Ok(result)
	}
}

impl Parser {
	pub fn function_parameters(input: Node) -> Result<FunctionParameters> {
		let mut children = input.children();

		let mut result = vec![];

		while let (Some(ident), Some(ty)) = (children.next(), children.next()) {
			let mut ident = Self::ident(ident);
			let ty: &'static TypeLayout = Self::r#type(ty)?;

			ident.link(input.user_data(), Some(Cow::Borrowed(ty)))?;

			result.push(ident);
		}

		Ok(FunctionParameters(result))
	}
}