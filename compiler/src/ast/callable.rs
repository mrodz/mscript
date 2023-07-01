use std::borrow::Cow;

use anyhow::{Context, Result, bail};

use crate::{
    instruction,
    parser::{Node, Parser}, scope::ScopeReturnStatus, VecErr,
};

use super::{
    map_err_messages, r#type::IntoType, Compile, CompiledItem, Dependencies, Dependency,
    FunctionArguments, Ident, TypeLayout,
};

#[derive(Debug)]
pub(crate) struct Callable {
    pub ident: Ident,
    pub function_arguments: FunctionArguments,
}

impl IntoType for Callable {
    fn for_type(&self) -> Result<TypeLayout> {
        let ident = self.ident.ty()?;
        let ident = ident.get_type_recursively();

        let (ScopeReturnStatus::Should(ref return_type) | ScopeReturnStatus::Did(ref return_type)) = ident.is_function().context("not a function")?.return_type.as_ref() else {
            bail!("function returns void")
        };

        let return_type_cloned: TypeLayout = return_type.as_ref().clone();

        Ok(return_type_cloned)
    }
}

static mut ARGUMENT_REGISTER: usize = 0;

struct ArgumentRegisterHandle(usize);

impl ArgumentRegisterHandle {
    pub fn new() -> Self {
        unsafe {
            ARGUMENT_REGISTER += 1;
            Self(ARGUMENT_REGISTER)
        }
    }

    pub fn repr_from_raw(raw: usize) -> String {
        format!("a#{raw}")
    }

    pub fn free(tally: usize) {
        unsafe {
            ARGUMENT_REGISTER = ARGUMENT_REGISTER.checked_sub(tally).expect("freeing too many registers");
        }
    }

    pub fn repr(&self) -> String {
        unsafe {
            assert!(self.0 <= ARGUMENT_REGISTER);
        }

        Self::repr_from_raw(self.0)
    }
}

impl Compile for Callable {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut register_start = None;
        let mut register_count = 0;

        let mut args_init: Vec<CompiledItem> = self
            .function_arguments
            .iter()
            .flat_map(|x| {
                let mut value_init = x.compile(function_buffer).unwrap();

                let argument_register = ArgumentRegisterHandle::new();
                value_init.push(instruction!(store (argument_register.repr())));

                if register_start.is_none() {
                    register_start = Some(argument_register.0);
                }

                register_count += 1;

                value_init
            })
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

        if let Some(register_start) = register_start {
            for register_idx in register_start..register_start+register_count {
                let name = ArgumentRegisterHandle::repr_from_raw(register_idx);
                args_init.push(instruction!(load_local name));
            }
        }

        args_init.push(load_instruction);
        args_init.push(instruction!(call));

        ArgumentRegisterHandle::free(register_count);

        Ok(args_init)
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
    pub fn callable(input: Node) -> Result<Callable, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();

        let user_data = input.user_data();

        let mut ident = Self::ident(ident).to_err_vec()?;

        if let Err(e) = ident.link_from_pointed_type_with_lookup(user_data) {
            return map_err_messages(
                Err(e),
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "unknown function".into(),
                || vec!["Attempting to call a function whose type is not known"],
            ).to_err_vec()
        }

        let function_arguments = children.next().unwrap();
        let function_arguments = Self::function_arguments(function_arguments)?;

        Ok(Callable {
            ident,
            function_arguments,
        })
    }
}
