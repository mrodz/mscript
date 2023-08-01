use std::{rc::Rc, borrow::Cow};

use anyhow::{Result, Context, bail};
use once_cell::sync::Lazy;
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::{parser::{AssocFileData, Node, Parser, Rule}, VecErr, ast::new_err, instruction};

use super::{list::Index, Dependencies, Ident, Value, r#type::IntoType, TypeLayout, Compile, value, TemporaryRegister};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    use pest::pratt_parser::Op;
    use Rule::*;

    PrattParser::new().op(Op::postfix(list_index))
});

#[derive(Debug)]
pub(crate) enum ReassignmentPath {
    Ident(Ident),
	ReferenceToSelf(Option<Cow<'static, TypeLayout>>),
    Index {
        lhs: Box<ReassignmentPath>,
        index: Index,
    },
}

impl Compile for ReassignmentPath {
	fn compile(&self, function_buffer: &mut Vec<super::CompiledItem>) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
		match self {
			ReassignmentPath::Ident(ident) => {
				ident.compile(function_buffer)
			}
			ReassignmentPath::ReferenceToSelf(_) => unimplemented!(),
			ReassignmentPath::Index { lhs, index } => {
				let mut result = lhs.compile(function_buffer)?;
	            result.append(&mut index.compile(function_buffer)?);
    	        Ok(result)
			}
		}
	}
}

impl IntoType for ReassignmentPath {
	fn for_type(&self) -> Result<super::TypeLayout> {
		match self {
			Self::Ident(ident) => ident.ty().map(|x| x.clone().into_owned()),
			Self::Index { index, .. } => index.for_type(),
			Self::ReferenceToSelf(Some(ty)) => Ok(ty.clone().into_owned()),
			Self::ReferenceToSelf(None) => bail!("`self` does not have a writable type in this context")
		}
	}
}

#[derive(Debug)]
pub(crate) struct Reassignment {
    path: ReassignmentPath,
    value: Value,
}

impl Compile for Reassignment {
	fn compile(&self, function_buffer: &mut Vec<super::CompiledItem>) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
		// todo!();
		let mut result = self.value.compile(function_buffer)?;

		let val_register = TemporaryRegister::new();

		result.push(instruction!(store val_register));

		result.append(&mut self.path.compile(function_buffer)?);

		result.append(&mut vec![
			instruction!(load_fast val_register),
			instruction!(vec_mut),
			instruction!(delete_name_scoped val_register)
		]);


		// result.append(&mut self.path.compile(function_buffer)?);
		
		Ok(result)
	}
}

fn parse_path(pairs: Pairs<Rule>, user_data: Rc<AssocFileData>) -> Result<ReassignmentPath, Vec<anyhow::Error>> {
    PRATT_PARSER
        .map_primary(|primary| {
            assert!(matches!(primary.as_rule(), Rule::ident));

			let raw_string = primary.as_str();

			if raw_string == "self" {
				// panic!()
				return Ok(ReassignmentPath::ReferenceToSelf(None));
				// return Ok(ReassignmentPath::ReferenceToSelf);
			}

			let file_name = user_data.get_file_name();

			let (ident, is_callback) = user_data
				.get_dependency_flags_from_name(&raw_string.to_string())
				.with_context(|| {
					new_err(
						primary.as_span(),
						&file_name,
						"use of undeclared variable".into(),
					)
				})
				.to_err_vec()?;

			let cloned = if is_callback {
				ident.clone().wrap_in_callback().to_err_vec()?
			} else {
				ident.clone()
			};
            // let ident = Parser::ident(Node::new_with_user_data(primary, Rc::clone(&user_data))).to_err_vec()?;

            Ok(ReassignmentPath::Ident(cloned))
        })
        .map_postfix(|lhs, op| match op.as_rule() {
			Rule::list_index => {
				let lhs = lhs?;

				let lhs_ty = lhs.for_type().to_err_vec()?;

				let index = Parser::list_index(Node::new_with_user_data(op, Rc::clone(&user_data)), lhs_ty)?;
				Ok(ReassignmentPath::Index { lhs: Box::new(lhs), index })
			},
			other => unimplemented!("{other:?}")
		})
		.parse(pairs)
}

impl ReassignmentPath {
    pub(crate) fn parse(input: Node) -> Result<ReassignmentPath, Vec<anyhow::Error>> {
		parse_path(input.children().into_pairs(), Rc::clone(input.user_data()))
	}
}

impl Dependencies for Reassignment {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.value.net_dependencies()
    }
}

impl Parser {
	pub fn reassignment(input: Node) -> Result<Reassignment, Vec<anyhow::Error>> {
		let mut children = input.children();
		let path = children.next().unwrap();
		let value = children.next().unwrap();

		let path = ReassignmentPath::parse(path)?;
		let value = Self::value(value)?;

		Ok(Reassignment { path, value })
	}
}