use anyhow::Result;

use crate::{
    ast::{CompilationState, Compile, CompiledItem, Dependencies, Ident, WalkForType},
    parser::{Node, Rule},
};

use super::{Constructor, MemberFunction, MemberVariable};

#[derive(Debug)]
pub(crate) enum ClassFeature {
    Function(MemberFunction),
    Variable(MemberVariable),
}

impl Compile for ClassFeature {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        match self {
            Self::Function(x) => x.compile(state),
            Self::Variable(x) => x.compile(state),
        }
    }
}

impl WalkForType for ClassFeature {
    fn type_from_node(input: &Node) -> Result<Ident> {
        use Rule as R;
        match input.as_rule() {
            R::class_variable => MemberVariable::type_from_node(input),
            R::class_bound_function => MemberFunction::type_from_node(input),
            R::class_constructor => Constructor::type_from_node(input),
            _ => unreachable!(),
        }
    }
}

impl Dependencies for ClassFeature {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        match self {
            Self::Function(x) => x.net_dependencies(),
            Self::Variable(x) => x.net_dependencies(),
        }
    }

    fn supplies(&self) -> Vec<crate::ast::Dependency> {
        match self {
            Self::Function(x) => x.supplies(),
            Self::Variable(x) => x.supplies(),
        }
    }
}
