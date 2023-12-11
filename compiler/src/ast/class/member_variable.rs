use std::borrow::Cow;

use anyhow::Result;

use crate::{
    ast::{
        assignment::AssignmentFlag, new_err, CompilationState, Compile, CompiledItem, Dependencies,
        Dependency, Ident, WalkForType,
    },
    instruction,
    parser::{Node, Parser, Rule},
    CompilationError, VecErr,
};

#[derive(Debug)]
pub(crate) struct MemberVariable {
    #[allow(unused)]
    flags: AssignmentFlag,
    ident: Ident,
}

impl Compile for MemberVariable {
    fn compile(&self, _: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let name = self.ident().name();

        Ok(vec![
            instruction!(reserve_primitive),
            instruction!(store_fast name),
        ])
    }
}

impl WalkForType for MemberVariable {
    fn type_from_node(input: &Node) -> Result<Ident> {
        let mut children = input.children();

        let maybe_ident = children.next().unwrap();

        let ident_node = if maybe_ident.as_rule() == Rule::ident {
            maybe_ident
        } else {
            children.next().unwrap()
        };

        let ty_node = children.next().details_lazy_message(
            input.as_span(),
            &input.user_data().get_source_file_name(),
            || {
                format!(
                    "when declaring a member variable, you must specify a type (eg. `{}: int`)",
                    ident_node.as_str()
                )
            },
        )?;

        let mut ident = Parser::ident(ident_node)?;

        let ty = Parser::r#type(ty_node)?;

        ident.link_force_no_inherit(input.user_data(), ty)?;

        Ok(ident)
    }
}

impl MemberVariable {
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl Dependencies for MemberVariable {
    fn supplies(&self) -> Vec<crate::ast::Dependency> {
        vec![Dependency::new(Cow::Borrowed(&self.ident))]
    }
}

impl Parser {
    pub fn class_variable(input: Node) -> Result<MemberVariable, Vec<anyhow::Error>> {
        let mut children = input.children();

        let either_flags_or_ident = children.next().unwrap();

        let (flags, mut ident) = if either_flags_or_ident.as_rule() == Rule::assignment_flags {
            let flags = Self::assignment_flags(either_flags_or_ident).to_err_vec()?;

            let ident = children.next().unwrap();
            let ident = Self::ident(ident).to_err_vec()?;

            (flags, ident)
        } else {
            let ident = either_flags_or_ident;
            let ident = Self::ident(ident).to_err_vec()?;

            (AssignmentFlag::default(), ident)
        };

        let Some(ty_node) = children.next() else {
            let var_name = ident.name();
            return Err(vec![new_err(input.as_span(), &input.user_data().get_file_name(), format!("member variables of a class require an explicit type (hint: change `{var_name}` to `{var_name}: type`", ))]);
        };

        let ty = Self::r#type(ty_node).to_err_vec()?;

        // link type to ident
        ident
            .link_force_no_inherit(input.user_data(), ty)
            .to_err_vec()?;

        Ok(MemberVariable { flags, ident })
    }
}
