use crate::{parser::{Parser, Node}, instruction, scope::ScopeReturnStatus};

use super::{Value, Block, Dependencies, Compile};

#[derive(Debug)]
pub struct WhileLoop {
	condition: Value,
	body: Block,
}

impl Compile for WhileLoop {
	fn compile(&self, function_buffer: &mut Vec<super::CompiledItem>) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
		let mut condition_compiled = self.condition.compile(function_buffer)?;
		let mut body_compiled = self.body.compile(function_buffer)?;

		let condition_len: isize = condition_compiled.len().try_into()?;
		let body_len: isize = body_compiled.len().try_into()?;

		condition_compiled.push(instruction!(while_loop (body_len + 2)));

		body_compiled.push(instruction!(jmp_pop (-(body_len + 1 + condition_len))));

		condition_compiled.append(&mut body_compiled);

		Ok(condition_compiled)
	}
}

impl Dependencies for WhileLoop {
	fn dependencies(&self) -> Vec<super::Dependency> {
		let mut condition_dependencies = self.condition.net_dependencies();
		condition_dependencies.append(&mut self.body.net_dependencies());
		condition_dependencies
	}
}

impl Parser {
	pub fn while_loop(input: Node) -> Result<WhileLoop, Vec<anyhow::Error>> {
		let mut children = input.children();

		let condition = children.next().unwrap();
		let block = children.next().unwrap();

		let condition = Self::value(condition)?;

		let child_returns_type = input
            .user_data()
            .return_statement_expected_yield_type()
            .map_or_else(
                || ScopeReturnStatus::No,
                |ty| ScopeReturnStatus::ParentShould(ty.clone()),
            );

		input.user_data().push_while_loop(child_returns_type);

		let body = Self::block(block)?;

		input.user_data().pop_scope();

		Ok(WhileLoop { condition, body })
	}
}