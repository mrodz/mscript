use std::borrow::Cow;

use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{new_err, Callable, Compile, FunctionArguments, TypeLayout};

#[derive(Debug)]
pub(crate) enum DotLookupOption {
    Name(String),
    FunctionCall {
        function_name: String,
        arguments: FunctionArguments,
    },
}

pub(crate) struct DotLookup<'a> {
    lookup_type: DotLookupOption,
    output_type: &'a TypeLayout,
}

#[derive(Debug)]
pub(crate) struct DotChain {
    links: Vec<DotLookupOption>,
}

impl Compile for DotLookupOption {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        match self {
            Self::Name(name) => Ok(vec![instruction!(lookup name)]),
            Self::FunctionCall {
                function_name,
                arguments,
            } => {
                let instance_register = state.poll_temporary_register();
                let lhs_register = state.poll_temporary_register();

                let mut result = vec![
                    instruction!(store_fast instance_register),
                    instruction!(load_fast instance_register),
                    instruction!(lookup function_name),
                    instruction!(store_fast lhs_register),
                ];

                let callable: Callable<'_> = Callable::new(
                    arguments,
                    instruction!(load_fast lhs_register),
                    Some(instance_register.to_string()),
                );

                result.append(&mut callable.compile(state)?);

                Ok(result)
            }
        }
    }
}

impl Compile for DotChain {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let mut result = vec![];
        for link in &self.links {
            result.append(&mut link.compile(state)?);
        }
        Ok(result)
    }
}

impl Parser {
    pub fn dot_chain<'a>(
        input: Node,
        mut lhs_ty: &'a TypeLayout,
    ) -> Result<(DotChain, &'a TypeLayout), Vec<anyhow::Error>> {
        let mut links = vec![];
        for dot_chain_option_node in input.children() {
            let dot_chain_option = Self::dot_chain_option(dot_chain_option_node, lhs_ty)?;

            links.push(dot_chain_option.lookup_type);

            lhs_ty = dot_chain_option.output_type;
        }

        Ok((DotChain { links }, lhs_ty))
    }

    pub fn dot_chain_option<'a>(
        input: Node,
        lhs_ty: &'a TypeLayout,
    ) -> Result<DotLookup<'a>, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let ident_str = ident.as_str().to_owned();
        let ident_span = ident.as_span();

        let source_name = input.user_data().get_source_file_name();

        let type_of_property = lhs_ty
            .get_property_type(&ident_str)
            .details_lazy_message(ident_span, &source_name, || {
                format!(
                    "this property does not exist on `{lhs_ty}`.{}",
                    lhs_ty.get_property_hint_from_input_no_lookup()
                )
            })
            .to_err_vec()?;

        match input.as_rule() {
            Rule::dot_function_call => {
                let Some(function_type) = type_of_property.is_function() else {
                    return Err(vec![new_err(
                        ident_span,
                        &source_name,
                        format!(
                            "this field has the type `{type_of_property}`, which is not callable"
                        ),
                    )]);
                };

                let arguments = children.next().unwrap().children().single().unwrap();

                assert_eq!(arguments.as_rule(), Rule::function_arguments);

                let arguments = Self::function_arguments(arguments, function_type.parameters(), Some(lhs_ty))?;

                let lookup_type = DotLookupOption::FunctionCall {
                    function_name: ident_str,
                    arguments,
                };

                let output_type = function_type.return_type().get_type().unwrap_or(&Cow::Owned(TypeLayout::Void));

                let output_type = if output_type.is_class_self() {
                    lhs_ty
                } else {
                    output_type
                };

                Ok(DotLookup {
                    lookup_type,
                    output_type,
                })
            }
            Rule::dot_name_lookup => Ok(DotLookup {
                lookup_type: DotLookupOption::Name(ident_str),
                output_type: type_of_property,
            }),
            x => unreachable!("{x:?}"),
        }
    }
}
