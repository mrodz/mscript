#[allow(unused_imports)]
use std::{borrow::Cow, sync::Arc};

#[allow(unused_imports)]
use anyhow::{Context, Result};

#[allow(unused_imports)]
use crate::{
    ast::FunctionParameters,
    instruction,
    parser::{Node, Parser},
    scope::ScopeType,
    VecErr,
};

#[allow(unused_imports)]
use super::{
    function::FunctionType, map_err_messages, r#type::IntoType, Compile, CompiledItem,
    Dependencies, Dependency, FunctionArguments, Ident, TemporaryRegister, TypeLayout,
};

enum CallableDestination {
    Standard {
        load_instruction: Box<dyn Fn() -> CompiledItem>,
    },
    ToSelf {
        return_type: Option<Cow<'static, TypeLayout>>,
    },
}

pub(crate) struct Callable<'a> {
    destination: CallableDestination,
    function_arguments: &'a FunctionArguments,
}

impl<'a> Callable<'a> {
    pub fn new_recursive_call(arguments: &'a FunctionArguments, return_type: Option<Cow<'static, TypeLayout>>) -> Self {
        Self {
            destination: CallableDestination::ToSelf { return_type },
            function_arguments: arguments,
        }
    }

    pub fn new<L>(arguments: &'a FunctionArguments, load_instruction: L) -> Self
    where
        L: (Fn() -> CompiledItem) + 'static
    {
        Self {
            destination: CallableDestination::Standard { load_instruction: Box::new(load_instruction) },
            function_arguments: arguments,
        }
    }
}

#[cfg(not)]
impl IntoType for Callable {
    /// return the return type
    fn for_type(&self) -> Result<TypeLayout> {
        static VOID_MSG: &str = "function returns void";
        match &self.destination {
            CallableDestination::Named { ident } => {
                let ty = ident.ty()?;
                let function_type = ty.is_function().context("not a function")?;
                let return_type = function_type.return_type().get_type().context(VOID_MSG)?;
                let return_type = return_type.clone().into_owned();
                Ok(return_type)
            }
            CallableDestination::ToSelf { return_type } => {
                return_type.clone().map(Cow::into_owned).context(VOID_MSG)
            }
        }
    }
}

impl Compile for Callable<'_> {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut register_start = None;
        let mut register_count = 0;

        let mut args_init: Vec<CompiledItem> = self
            .function_arguments
            .iter()
            .flat_map(|x| {
                let mut value_init = x.compile(function_buffer).unwrap();

                let argument_register = TemporaryRegister::new_require_explicit_drop();
                value_init.push(instruction!(store_fast argument_register));

                if register_start.is_none() {
                    register_start = Some(argument_register.0);
                }

                register_count += 1;

                value_init
            })
            .collect();

        if let Some(register_start) = register_start {
            for register_idx in register_start..register_start + register_count {
                unsafe {
                    let name = TemporaryRegister::new_ghost_register(register_idx);
                    args_init.push(instruction!(load_fast name));
                }
            }
        }

        unsafe {
            TemporaryRegister::free_many(register_count);
        }

        let CallableDestination::Standard { load_instruction } = &self.destination else {
            // let CallableDestination::ToSelf { return_type }
            args_init.push(instruction!(call_self));

            return Ok(args_init);
        };

        // let func_name = ident.name();

        // let load_instruction = match ident.ty()? {
        //     Cow::Owned(TypeLayout::CallbackVariable(..))
        //     | Cow::Borrowed(TypeLayout::CallbackVariable(..)) => {
        //         instruction!(load_callback func_name)
        //     }
        //     _ => instruction!(load func_name),
        // };

        let x = (&load_instruction)();

        args_init.push(load_instruction());
        args_init.push(instruction!(call));

        Ok(args_init)
    }
}

#[cfg(not)]
impl Dependencies for Callable {
    fn dependencies(&self) -> Vec<Dependency> {
        // a call needs to have access to the function/object
        // println!("Function Call");
        // let mut maybe_arg_dependencies = self.function_arguments.net_dependencies();

        // if let CallableDestination::Named { ident } = &self.destination {
        //     maybe_arg_dependencies.push(Dependency::new(Cow::Borrowed(ident)));
        // }

        // maybe_arg_dependencies
    }
}

#[cfg(not)]
impl Parser {
    #[deprecated(note = "Callable has been shifted from an atom to a binary operation postfix")]
    pub fn callable(input: Node) -> Result<Callable, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();

        let user_data = input.user_data();

        // let destination: CallableDestination = if ident.as_str() != "self" {
            
        //     CallableDestination::Named { ident }
        // } else {
        //     let return_type: Option<&Cow<'_, TypeLayout>> =
        //         user_data.return_statement_expected_yield_type();

        //     CallableDestination::ToSelf { return_type }
        // };

        let function_arguments = children.next().unwrap();

        enum Choice {
            Named(Arc<FunctionType>),
            ToSelf(Arc<FunctionParameters>),
        }

        impl Choice {
            fn get_parameters(&self) -> &FunctionParameters {
                match self {
                    Self::Named(function_type) => &function_type.parameters(),
                    Self::ToSelf(function_parameters) => function_parameters,
                }
            }
        }

        let (parameters_union, destination) = if ident.as_str() == "self" {
            let function_scope = input
                .user_data()
                .get_current_executing_function()
                .context("no function scope found for `self` call.")
                .to_err_vec()?;

            function_scope.ty_ref();

            let ScopeType::Function(Some(parameters)) = function_scope.ty_ref() else {
                unreachable!();
            };

            let ty = function_scope.peek_yields_value().get_type().cloned();

            let parameters = Choice::ToSelf(parameters.clone());
            let destination = CallableDestination::ToSelf { return_type: ty };

            (parameters, destination)
        } else {
            let mut ident = Self::ident(ident).to_err_vec()?;

            if let Err(e) = ident.link_from_pointed_type_with_lookup(user_data) {
                return map_err_messages(
                    Err(e),
                    input.as_span(),
                    &input.user_data().get_source_file_name(),
                    "unknown function".into(),
                    || vec!["Attempting to call a function whose type is not known"],
                )
                .to_err_vec();
            }

            // lookup the identity of the function
            let type_layout = ident.ty().to_err_vec()?;

            let function_type = type_layout
                .is_function()
                .context("not a function")
                .to_err_vec()?;

            let parameters = Choice::Named(function_type);
            let destination = CallableDestination::Named { ident };

            (parameters, destination)
        };

        let function_arguments =
            Self::function_arguments(function_arguments, parameters_union.get_parameters())?;

        Ok(Callable {
            destination,
            function_arguments,
        })
    }
}
