use std::{sync::Arc, borrow::Cow};

use anyhow::Result;

use crate::{ast::{Ident, FunctionParameters, Block, Dependencies, function::FunctionType, TypeLayout}, parser::{Parser, Node, Rule}, VecErr, scope::ScopeReturnStatus};

use super::WalkForType;

#[derive(Debug)]
pub(crate) struct MemberFunction {
	ident: Ident,
	parameters: FunctionParameters,
	body: Block,
}

impl WalkForType for MemberFunction {
	fn type_from_node(input: &Node) -> Result<Ident> {
		let mut children = input.children();

		let ident_node = children.next().unwrap();
		let mut ident = Parser::ident(ident_node)?;

		let parameters_node = children.next().unwrap();
		let parameters = Parser::function_parameters(parameters_node, false)?;

		let maybe_return_type_node = children.next().unwrap();

		let return_type = if maybe_return_type_node.as_rule() == Rule::function_return_type {
			ScopeReturnStatus::Should(Parser::function_return_type(maybe_return_type_node)?)
		} else {
			ScopeReturnStatus::Void
		};

		let function_type = FunctionType::new(Arc::new(parameters), return_type);

		ident.link_force_no_inherit(input.user_data(), Cow::Owned(TypeLayout::Function(function_type)))?;

		Ok(ident)
	}
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