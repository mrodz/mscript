use anyhow::Result;

use crate::{
    parser::{Node, Parser, Rule},
    CompilationError, VecErr,
};

use super::{new_err, FunctionArguments, TypeLayout};

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

impl Parser {
	pub fn dot_chain<'a>(input: Node, mut lhs_ty: &'a TypeLayout) -> Result<(DotChain, &'a TypeLayout), Vec<anyhow::Error>> {
		let mut links = vec![];
		for dot_chain_option_node in input.children() {
			let dot_chain_option = Self::dot_chain_option(dot_chain_option_node, lhs_ty)?;

			links.push(dot_chain_option.lookup_type);

			lhs_ty = dot_chain_option.output_type;
		}

		Ok((DotChain { links }, lhs_ty))
	}

    pub fn dot_chain_option<'a>(input: Node, lhs_ty: &'a TypeLayout) -> Result<DotLookup<'a>, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let ident_str = ident.as_str().to_owned();
        let ident_span = ident.as_span();

        let source_name = input.user_data().get_source_file_name();

        let type_of_property = lhs_ty.get_property_type(&ident_str).details(
            ident_span,
            &source_name,
            format!("this property does not exist on `{lhs_ty}`"),
        ).to_err_vec()?;

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

                let arguments = children.next().unwrap();

                let arguments = Self::function_arguments(arguments, function_type.parameters())?;

				let lookup_type = DotLookupOption::FunctionCall { function_name: ident_str, arguments };

				Ok(DotLookup { lookup_type, output_type: type_of_property })
            }
            Rule::dot_name_lookup => Ok(DotLookup {
				lookup_type: DotLookupOption::Name(ident_str),
				output_type: type_of_property,
			}),
			x => unreachable!("{x:?}")
        }
    }
}
