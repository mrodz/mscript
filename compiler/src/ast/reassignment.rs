use std::{borrow::Cow, rc::Rc};

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use pest::{iterators::Pairs, pratt_parser::PrattParser};

use crate::{
    ast::new_err,
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{
    dot_lookup::DotChain,
    list::Index,
    r#type::{IntoType, TypecheckFlags},
    CompilationState, Compile, Dependencies, Ident, TypeLayout, Value,
};

pub static PRATT_PARSER: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    use pest::pratt_parser::Op;
    use Rule as R;

    PrattParser::new().op(Op::postfix(R::list_index) | Op::postfix(R::dot_chain))
});

#[derive(Debug)]
pub(crate) enum ReassignmentPath {
    Ident(Ident),
    ReferenceToSelf(Option<Cow<'static, TypeLayout>>),
    Index {
        lhs: Box<ReassignmentPath>,
        index: Index,
    },
    DotLookup {
        lhs: Box<ReassignmentPath>,
        dot_chain: DotChain,
        expected_type: TypeLayout,
    },
}

impl Compile for ReassignmentPath {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        match self {
            ReassignmentPath::Ident(ident) => ident.compile(state),
            ReassignmentPath::ReferenceToSelf(_) => Ok(vec![instruction!(load_fast "self")]),
            ReassignmentPath::Index { lhs, index } => {
                let mut result = lhs.compile(state)?;
                result.append(&mut index.compile(state)?);
                Ok(result)
            }
            ReassignmentPath::DotLookup { lhs, dot_chain, .. } => {
                let mut result = lhs.compile(state)?;

                result.append(&mut dot_chain.compile(state)?);
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
            Self::ReferenceToSelf(None) => {
                bail!("`self` does not have a writable type in this context")
            }
            Self::DotLookup { .. } => todo!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Reassignment {
    path: ReassignmentPath,
    value: Value,
}

impl Compile for Reassignment {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let mut result = self.value.compile(state)?;

        let val_register = state.poll_temporary_register();

        result.push(instruction!(store val_register));

        result.append(&mut self.path.compile(state)?);

        result.append(&mut vec![
            instruction!(load_fast val_register),
            instruction!(ptr_mut),
            instruction!(delete_name_scoped val_register),
        ]);

        // result.append(&mut self.path.compile(function_buffer)?);

        Ok(result)
    }
}

fn parse_path(
    pairs: Pairs<Rule>,
    user_data: Rc<AssocFileData>,
) -> Result<ReassignmentPath, Vec<anyhow::Error>> {
    PRATT_PARSER
        .map_primary(|primary| {
            assert!(matches!(primary.as_rule(), Rule::ident));

            let raw_string = primary.as_str();

            if raw_string == "self" {
                if let Some(class_type) = user_data.get_type_of_executing_class() {
                    // cloning ClassType is cheap
                    let class_type = class_type.clone();
                    return Ok((
                        ReassignmentPath::ReferenceToSelf(Some(Cow::Owned(TypeLayout::Class(
                            class_type,
                        )))),
                        primary.as_span(),
                    ));
                }

                return Ok((ReassignmentPath::ReferenceToSelf(None), primary.as_span()));
            }

            let file_name = user_data.get_source_file_name();

            let (ident, is_callback) = user_data
                .get_dependency_flags_from_name(raw_string)
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

            Ok((ReassignmentPath::Ident(cloned), primary.as_span()))
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::list_index => {
                let (lhs, lhs_span) = lhs?;

                let lhs_ty = lhs
                    .for_type()
                    .details(lhs_span, &user_data.get_source_file_name(), "Invalid index")
                    .to_err_vec()?;

                let index = Parser::list_index(
                    Node::new_with_user_data(op, Rc::clone(&user_data)),
                    lhs_ty,
                )?;
                Ok((
                    ReassignmentPath::Index {
                        lhs: Box::new(lhs),
                        index,
                    },
                    lhs_span,
                ))
            }
            Rule::dot_chain => {
                let (lhs, lhs_span) = lhs?;

                let lhs_ty = lhs
                    .for_type()
                    .details(
                        lhs_span,
                        &user_data.get_source_file_name(),
                        "Invalid lookup",
                    )
                    .to_err_vec()?;
                let lhs_ty = lhs_ty.assume_type_of_self(&user_data);

                let (dot_chain, expected_type) = Parser::dot_chain(
                    Node::new_with_user_data(op, Rc::clone(&user_data)),
                    Cow::Borrowed(&lhs_ty),
                )?;

                Ok((
                    ReassignmentPath::DotLookup {
                        lhs: Box::new(lhs),
                        dot_chain,
                        expected_type: expected_type.into_owned(),
                    },
                    lhs_span,
                ))
            }
            other => unimplemented!("{other:?}"),
        })
        .parse(pairs)
        .map(|x| x.0)
}

impl ReassignmentPath {
    pub(crate) fn parse(input: Node) -> Result<ReassignmentPath, Vec<anyhow::Error>> {
        parse_path(input.children().into_pairs(), Rc::clone(input.user_data()))
    }

    pub(crate) fn expected_type(&self) -> &TypeLayout {
        match self {
            Self::DotLookup { expected_type, .. } => expected_type,
            Self::ReferenceToSelf(Some(ty)) => ty.as_ref(),
            Self::ReferenceToSelf(None) => unreachable!("`self` does not have a type here"),
            Self::Ident(ident) => ident.ty().unwrap().as_ref(),
            Self::Index { index, .. } => index.final_output_type(),
        }
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

        let path_span = value.as_span();

        let path = ReassignmentPath::parse(path)?;
        let value = Self::value(value)?;
        let value_ty = value.for_type().to_err_vec()?;

        let expected_ty = path.expected_type();

        if !value_ty.eq_complex(
            expected_ty,
            &TypecheckFlags::use_class(input.user_data().get_type_of_executing_class()),
        ) {
            return Err(vec![new_err(
                path_span,
                &input.user_data().get_source_file_name(),
                format!("type mismatch: cannot assign `{value_ty:?}` to `{expected_ty:?}`"),
            )]);
        }

        Ok(Reassignment { path, value })
    }
}
