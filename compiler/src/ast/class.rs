mod class_body;
mod class_feature;
mod constructor;
mod member_function;
mod member_variable;

use std::{borrow::Cow, sync::Arc, fmt::Display};

use anyhow::Result;

pub(crate) use class_body::ClassBody;
pub(crate) use constructor::Constructor;
pub(crate) use member_function::MemberFunction;
pub(crate) use member_variable::MemberVariable;

use crate::{parser::{Parser, Node}, VecErr, ast::TypeLayout};

use super::{Dependencies, Ident, Dependency, r#type::IntoType, Compile};

#[derive(Debug)]
pub struct Class {
    ident: Ident,
    body: ClassBody,
}

impl Compile for Class {
	fn compile(&self, state: &super::CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
		todo!()
	}
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ClassType {
	name: Arc<String>,
	fields: Arc<[Ident]>,
}

impl ClassType {
	pub fn name(&self) -> &str {
		&self.name
	}
}

impl Display for ClassType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut builder = f.debug_struct(&("class ".to_owned() + &self.name));

		for field in self.fields.iter() {
			builder.field(&field.name(), field.ty().unwrap());
		}

		builder.finish()
	}
}

impl IntoType for Class {
	fn for_type(&self) -> Result<super::TypeLayout> {
		Ok(self.ident.ty()?.clone().into_owned())
	}
}

impl Dependencies for Class {
	fn dependencies(&self) -> Vec<Dependency> {
		self.body.net_dependencies()
	}

	fn supplies(&self) -> Vec<Dependency> {
		vec![Dependency::new(Cow::Borrowed(&self.ident))]
	}
}

impl Parser {
	pub fn class(input: Node) -> Result<Class, Vec<anyhow::Error>> {
		let mut children = input.children();

		let ident_node = children.next().unwrap();

		let mut ident = Self::ident(ident_node).to_err_vec()?;

		let body_node = children.next().unwrap();

		// let name = name_node.as_str().to_owned();

		log::trace!("+class {}", ident.name());

		let class_scope = input
            .user_data()
            .push_class();

		let body = Self::class_body(body_node)?;

		let fields: Arc<[Ident]> = body.into_idents().into();
		let class_type = ClassType { name: Arc::new(ident.name().to_owned()), fields };

		ident.link_force_no_inherit(input.user_data(), Cow::Owned(TypeLayout::Class(class_type))).to_err_vec()?;

		let result = Class { ident, body };

		println!("{}", result.ident.ty().unwrap());

		Ok(result)
	}
}