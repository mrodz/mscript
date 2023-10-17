use std::borrow::Cow;

use anyhow::bail;

use crate::{parser::{Parser, Node}, VecErr, ast::{r#type::IntoType, new_err}};

use super::{Dependencies, Compile, Ident, Value};

#[derive(Debug)]
pub(crate) struct UnwrapExpr {
	ident: Ident,
	value: Box<Value>,
}

impl Dependencies for UnwrapExpr {
	
}

impl Compile for UnwrapExpr {
	fn compile(&self, state: &super::CompilationState) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
		todo!()
	}
}

impl Parser {
	pub fn unwrap_expr(input: Node) -> Result<UnwrapExpr, Vec<anyhow::Error>> {
		let mut children = input.children();

		let ident = children.next().unwrap();
		let value = children.next().unwrap();

		let value_span = value.as_span();

		dbg!(ident.as_rule());
		dbg!(value.as_rule());

		let mut ident = Self::ident(ident).to_err_vec()?;

		let value = Self::value(value)?;

		let value_ty = value.for_type().to_err_vec()?;

		let user_data = input.user_data();

		let optional_check = value_ty.is_optional();

		match optional_check {
			(true, Some(known_type)) => ident.link_force_no_inherit(&user_data, known_type.clone()).to_err_vec()?,
			(true, _) => return Err(vec![new_err(value_span, &user_data.get_source_file_name(), "the type of this optional is unknown".to_owned())]),
			_ => ident.link_force_no_inherit(&user_data, Cow::Owned(value_ty)).to_err_vec()?,
		}

		Ok(UnwrapExpr { ident, value: Box::new(value) })
		
	}
}