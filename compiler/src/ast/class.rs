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

pub(in crate::ast::class) trait WalkForType {
	fn type_from_node(input: &Node) -> Result<Ident>;
}

#[derive(Debug)]
pub struct Class {
    ident: Ident,
    body: ClassBody,
}

impl Compile for Class {
	fn compile(&self, state: &super::CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
		let mut _body_compiled = self.body.compile(state);

		todo!();
	}
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct ClassType {
	name: Arc<String>,
	fields: Arc<[Ident]>,
	id: usize,
}

impl ClassType {
	pub fn name(&self) -> &str {
		&self.name
	}

	pub(crate) fn arced_name(&self) -> Arc<String> {
		Arc::clone(&self.name)
	}

	pub fn fields(&self) -> &[Ident] {
		&self.fields
	}

	pub(crate) fn arced_fields(&self) -> Arc<[Ident]> {
		Arc::clone(&self.fields)
	}

	pub fn get_property<'a>(&'a self, name: &str) -> Option<&'a Ident> {
		for field in self.fields.iter() {
			if field.name() == name {
				return Some(field);
			}
		}

		None
	}
}

impl Display for ClassType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "class {} {{ ", self.name())?;

		let mut fields = self.fields.iter();

		if let Some(field) = fields.next() {
			write!(f, "{}", field)?;
		}

		for field in fields {
			write!(f, ", {field}")?;
		}

		write!(f, " }}")
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

		let _class_scope = input.user_data().push_class_unknown_self();

		let fields = ClassBody::get_members(&body_node).to_err_vec()?;

		let class_type = ClassType {
			fields,
			name: Arc::new(ident.name().to_owned()),
			id: input.user_data().request_class_id()
		};

		ident.link_force_no_inherit(input.user_data(), Cow::Owned(TypeLayout::Class(class_type.clone()))).to_err_vec()?;

		log::trace!("+class {}", ident.name());

		input
            .user_data()
            .set_self_type_of_class(class_type);

		let body = Self::class_body(body_node)?;

		let result = Class { ident, body };

		println!("{}", result.ident.ty().unwrap());

		Ok(result)
	}
}