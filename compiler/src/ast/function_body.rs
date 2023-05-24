use anyhow::Result;

use crate::{parser::{Node, Parser}, instruction};

use super::{Dependencies, Declaration, Compile, CompiledItem, Dependency};

#[derive(Debug, Clone)]
pub struct FunctionBody(Vec<Declaration>);

impl FunctionBody {
	pub fn empty_body() -> Self {
		Self(vec![])
	}
}

impl Dependencies for FunctionBody {
	fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
		dbg!("_____:)_____");
		let x: Vec<Dependency> = self.0.iter()
			.filter_map(|x| x.get_dependencies())
			.flat_map(|x| x.into_vec())
			.collect();

		let x = x.into_boxed_slice();

		if x.is_empty() {
			None
		} else {
			Some(x)
		}

	}
}

impl Compile for FunctionBody {
	fn compile(&self) -> Result<Vec<super::CompiledItem>> {
		let mut compiled_body: Vec<super::CompiledItem> = self.0.iter()
			.flat_map(|x| x.compile().unwrap())
			// .flatten()
			.collect();

		const RET: u8 = 0x12;

		if let Some(CompiledItem::Instruction { id: RET, .. }) = compiled_body.last() {
			// nothing! the function returns by itself
		} else {
			compiled_body.push(instruction!(void));
			compiled_body.push(instruction!(ret));
		}

		Ok(compiled_body)
	}
}

impl Parser {
	pub fn function_body(input: Node) -> Result<FunctionBody> {
		let children = input.children();

		let mut result = vec![];

		for child in children {
			result.push(Self::declaration(child)?)
		}

		Ok(FunctionBody(result))
	}
}