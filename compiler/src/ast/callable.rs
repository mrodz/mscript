use std::borrow::Cow;

use anyhow::{Context, Result, bail};

use crate::{
    instruction,
    parser::{Node, Parser},
};

use super::{
    map_err_messages, r#type::IntoType, Compile, CompiledItem, Dependencies, Dependency,
    FunctionArguments, Ident, TypeLayout,
};

#[derive(Debug, Clone)]
pub(crate) struct Callable {
    pub ident: Ident,
    pub function_arguments: FunctionArguments,
}

impl IntoType for Callable {
    fn into_type(&self) -> Result<TypeLayout> {
        let ident = self.ident.ty()?;
        let ident = ident.get_type_recursively();

        let Some(box ref return_type) = ident.is_function().context("not a function")?.return_type else {
            bail!("function returns void")
        };

        let return_type_cloned: TypeLayout = return_type.as_ref().clone();

        Ok(return_type_cloned)
    }
}

impl Compile for Callable {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut args: Vec<CompiledItem> = self
            .function_arguments
            .iter()
            .flat_map(|x| x.compile(function_buffer).unwrap())
            // .flatten()
            .collect();

        let func_name = self.ident.name();
        let ident = &self.ident;

        let load_instruction = match ident.ty()? {
            Cow::Owned(TypeLayout::CallbackVariable(..))
            | Cow::Borrowed(TypeLayout::CallbackVariable(..)) => {
                instruction!(load_callback func_name)
            }
            _ => instruction!(load func_name),
        };

        args.push(load_instruction);
        args.push(instruction!(call));

        Ok(args)
    }
}

impl Dependencies for Callable {
    fn dependencies(&self) -> Vec<Dependency> {
        // a call needs to have access to the function/object
        // println!("Function Call");
        let mut maybe_arg_dependencies = self.function_arguments.net_dependencies();

        maybe_arg_dependencies.push(Dependency::new(Cow::Borrowed(&self.ident)));

        maybe_arg_dependencies
    }
}

impl Parser {
    pub fn callable(input: Node) -> Result<Callable> {
        let mut children = input.children();

        let ident = children.next().unwrap();

        let user_data = input.user_data();

        let mut ident = Self::ident(ident)?;

        let maybe = ident.link_from_pointed_type_with_lookup(user_data);

        map_err_messages(
            maybe,
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "unknown function".into(),
            || vec!["Attempting to call a function whose type is not known"],
        )?;

        let function_arguments = children.next().unwrap();
        let function_arguments = Self::function_arguments(function_arguments)?;

        Ok(Callable {
            ident,
            function_arguments,
        })
    }
}
