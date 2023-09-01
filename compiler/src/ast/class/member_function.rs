use anyhow::Result;

use crate::{ast::{Ident, FunctionParameters, Block, Dependencies}, parser::{Parser, Node}, VecErr};

#[derive(Debug)]
pub(crate) struct MemberFunction {
	ident: Ident,
	parameters: FunctionParameters,
	body: Block,
}

impl MemberFunction {
	pub fn ident(&self) -> &Ident {
		&self.ident
	}
}

impl Dependencies for MemberFunction {
	fn dependencies(&self) -> Vec<crate::ast::Dependency> {
		self.body.net_dependencies()
	}

	fn supplies(&self) -> Vec<crate::ast::Dependency> {
		self.parameters.supplies()
	}
}

impl Parser {
	pub fn class_bound_function(input: Node) -> Result<MemberFunction, Vec<anyhow::Error>> {
		let mut children = input.children();

		let ident = children.next().unwrap();
		let ident = Self::ident(ident).to_err_vec()?;

		let parameters = children.next().unwrap();
		let parameters = Self::function_parameters(parameters, true).to_err_vec()?;

		let body = children.next().unwrap();
		let body = Self::block(body)?;

		Ok(MemberFunction { ident, parameters, body })
	}
}