use anyhow::Result;

use crate::instruction;

use super::{CompilationState, Compile, CompiledItem, FunctionArguments, TemporaryRegister};

enum CallableDestination {
    Standard {
        load_instruction: CompiledItem,
        self_register: Option<String>,
    },
    ToSelf,
}

pub(crate) struct Callable<'a> {
    destination: CallableDestination,
    function_arguments: &'a FunctionArguments,
}

impl<'a> Callable<'a> {
    pub const fn new_recursive_call(arguments: &'a FunctionArguments) -> Self {
        Self {
            destination: CallableDestination::ToSelf,
            function_arguments: arguments,
        }
    }

    pub const fn new(
        arguments: &'a FunctionArguments,
        load_instruction: CompiledItem,
        self_register: Option<String>,
    ) -> Self {
        Self {
            destination: CallableDestination::Standard {
                load_instruction,
                self_register,
            },
            function_arguments: arguments,
        }
    }
}

impl Compile for Callable<'_> {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        let mut register_start = None;
        let mut register_count = 0;

        let mut args_init: Vec<CompiledItem> = self
            .function_arguments
            .iter()
            .flat_map(|x| {
                let mut value_init = x.compile(state).unwrap();

                let argument_register = unsafe { state.poll_temporary_register_ghost() };

                value_init.push(instruction!(store_fast argument_register));

                if register_start.is_none() {
                    register_start = Some(argument_register.id);
                }

                register_count += 1;

                value_init
            })
            .collect();

        #[cfg(feature = "debug")]
        {
            args_init.insert(0, instruction!(breakpoint "CALL PREINIT args"));
            args_init.push(instruction!(breakpoint "CALL POSTINIT args"));
        }

        if let Some(register_start) = register_start {
            for register_idx in register_start..register_start + register_count {
                unsafe {
                    let name = TemporaryRegister::new_ghost_register(register_idx);
                    args_init.push(instruction!(load_fast name));
                }
            }
        }

        unsafe {
            state.free_many_temporary_registers(register_count);
        }

        let CallableDestination::Standard {
            load_instruction,
            self_register,
        } = &self.destination
        else {
            // let CallableDestination::ToSelf { return_type }
            args_init.push(instruction!(call_self));

            return Ok(args_init);
        };

        if let Some(name) = self_register {
            args_init.push(instruction!(ld_self name));
        }

        args_init.push(load_instruction.clone());
        args_init.push(instruction!(call));

        Ok(args_init)
    }
}
