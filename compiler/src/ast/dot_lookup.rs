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
        assume_self_is_on_top: bool,
    },
}

pub(crate) struct DotLookup<'a> {
    lookup_type: DotLookupOption,
    output_type: Cow<'a, TypeLayout>,
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
                assume_self_is_on_top,
            } => {
                let instance_register = state.poll_temporary_register();
                let lhs_register = state.poll_temporary_register();

                let mut result = vec![/*instruction!(breakpoint "TOP DotLookupOption")*/];

                if *assume_self_is_on_top {
                    result.extend_from_slice(&[
                        instruction!(store_fast instance_register),
                        instruction!(load_fast instance_register),
                    ]);
                }

                result.extend_from_slice(&[
                    instruction!(lookup function_name),
                    instruction!(store_fast lhs_register),
                    // instruction!(breakpoint "POST store function name in lhs_register"),
                ]);

                // let load_register = if *assume_self_is_on_top {
                //     &instance_register
                // } else {
                //     &lhs_register
                // };

                let callable: Callable<'_> = Callable::new(
                    arguments,
                    instruction!(load_fast lhs_register),
                    if *assume_self_is_on_top {
                        Some(instance_register.to_string())
                    } else {
                        None
                        // Some(lhs_register.to_string())
                    },
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
        mut lhs_ty: Cow<'a, TypeLayout>,
    ) -> Result<(DotChain, Cow<'a, TypeLayout>), Vec<anyhow::Error>> {
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
        lhs_ty: Cow<'a, TypeLayout>,
    ) -> Result<DotLookup<'a>, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let ident_str = ident.as_str().to_owned();

        if ident_str == "to_string" {
            // panic!("{lhs_ty:?}")
        }

        let ident_span = ident.as_span();

        let source_name = input.user_data().get_source_file_name();

        dbg!(&lhs_ty);

        let type_of_property = lhs_ty
            .get_property_type(&ident_str)
            .details_lazy_message(ident_span, &source_name, || {
                format!(
                    "this property does not exist on `{lhs_ty}`.{}",
                    lhs_ty.get_property_hint_from_input_no_lookup()
                )
            })
            .to_err_vec()?
            .clone();

        match input.as_rule() {
            Rule::dot_function_call => {
                let Some(function_type) = type_of_property.is_callable() else {
                    return Err(vec![new_err(
                        ident_span,
                        &source_name,
                        format!(
                            "this field has the type `{}`, which is not callable",
                            type_of_property
                        ),
                    )]);
                };

                let arguments = children.next().unwrap().children().single().unwrap();

                assert_eq!(arguments.as_rule(), Rule::function_arguments);

                let mut allow_self_type = Cow::Borrowed(lhs_ty.as_ref());
                let mut assume_self_is_on_top = true;

                if let TypeLayout::Module(module_type) = lhs_ty.as_ref() {
                    if let Some(ident) = module_type.get_property(&ident_str) {
                        let ident_ty = ident.ty().expect("ident should have type");

                        assume_self_is_on_top = false;

                        if ident_ty.is_class() {
                            allow_self_type = Cow::Owned(ident_ty.clone().into_owned());
                        } else {
                            let callable_ty = ident_ty
                                .is_callable()
                                .details(
                                    input.as_span(),
                                    &input.user_data().get_source_file_name(),
                                    "this is not callable",
                                )
                                .to_err_vec()?;

                            allow_self_type =
                                Cow::Owned(TypeLayout::Function(callable_ty.into_owned()));
                        }
                    }
                }

                let arguments = Self::function_arguments(
                    arguments,
                    function_type.parameters(),
                    if assume_self_is_on_top {
                        Some(allow_self_type.as_ref())
                    } else {
                        None
                    },
                )?;

                let lookup_type = DotLookupOption::FunctionCall {
                    function_name: ident_str,
                    arguments,
                    assume_self_is_on_top,
                };

                let output_type = function_type
                    .return_type()
                    .get_type()
                    .unwrap_or(&Cow::Owned(TypeLayout::Void));

                // drop(type_of_property);

                let output_type = if output_type.is_class_self() {
                    lhs_ty
                } else {
                    output_type.clone()
                };

                Ok(DotLookup {
                    lookup_type,
                    output_type,
                })
            }
            Rule::dot_name_lookup => Ok(DotLookup {
                lookup_type: DotLookupOption::Name(ident_str),
                output_type: type_of_property.clone(),
            }),
            x => unreachable!("{x:?}"),
        }
    }
}
