use crate::ast::Dependencies;

use super::{Constructor, MemberFunction, MemberVariable};

#[derive(Debug)]
pub(crate) enum ClassFeature {
	Constructor(Constructor),
	Function(MemberFunction),
	Variable(MemberVariable),
}

impl Dependencies for ClassFeature {
	fn dependencies(&self) -> Vec<crate::ast::Dependency> {
		match self {
			Self::Constructor(x) => x.net_dependencies(),
			Self::Function(x) => x.net_dependencies(),
			Self::Variable(x) => x.net_dependencies(),
		}
	}

	fn supplies(&self) -> Vec<crate::ast::Dependency> {
		match self {
			Self::Constructor(x) => x.supplies(),
			Self::Function(x) => x.supplies(),
			Self::Variable(x) => x.supplies(),
		}
	}
}