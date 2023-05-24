use anyhow::{Result, bail};

use crate::{parser::{Node, Parser, Rule}, instruction, ast::{CompiledItem, r#type::IntoType}};

use super::{value::Value, Ident, Dependencies, Compile, Dependency};

#[derive(Debug, Clone)]
pub(crate) struct Assignment {
    pub ident: Ident,
    pub value: Value,
}

impl Dependencies for Assignment {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        match self.value {
            Value::Ident(ref name) => {
                name.get_dependencies()
            }
            Value::Function(ref function) => {
                function.get_dependencies()
            }
            Value::Number(ref number) => {
                number.get_dependencies()
            }
        }
    }
}

impl Compile for Assignment {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let name = &self.ident.name();

        let matched = match &self.value {
            Value::Ident(ident) => {

                vec![
                    instruction!(load ident),
                    instruction!(store name)
                ]
            }
            Value::Function(function) => {
                let compiled_function = function.compile()?.remove(0);
                let CompiledItem::Function { ref id, .. } = compiled_function else {
                    unreachable!()
                };

                // let function_name = name_from_function_id(id);

                let dependencies = function.get_dependencies();

                let (dependencies, len) = if let Some(dependencies) = dependencies {
                    let len = dependencies.len() + 1;
                    (dependencies, len)
                } else {
                    ([].into(), 1)
                };

                let mut arguments = Vec::with_capacity(len);

                arguments.push(id.to_string());

                dependencies
                    .iter()
                    .map(|ident| ident.to_string())
                    .collect_into(&mut arguments);

                let arguments = arguments.into_boxed_slice();

                // as per `bytecode/src/instruction_constants.rs`
                const MAKE_FUNCTION: u8 = 0x0D;

                let make_function_instruction = CompiledItem::Instruction {
                    id: MAKE_FUNCTION, arguments
                };

                vec![
                    compiled_function,
                    make_function_instruction,
                    instruction!(store name)
                ]
            }
            Value::Number(number) => {
                let mut number_init = number.compile()?;

                number_init.push(instruction!(store name));

                number_init
            }
        };

        Ok(matched)
    }
}

impl Parser {
    pub fn assignment(input: Node) -> Result<Assignment> {
        let child = input.children().next().unwrap();
        let x = match child.as_rule() {
            Rule::assignment_no_type => Self::assignment_no_type(child)?,
            Rule::assignment_type => unimplemented!(),
            _ => unreachable!()
        };

        Ok(x)
        // let mut children = input.children();

        // let (Some(ident), Some(rhs)) = (children.next(), children.next()) else {
		// 	unreachable!()
		// };

        // let mut ident = Self::ident(ident);
        // // let (mut ident, flags) = (packed.0, packed.1);

        // let value = Self::value(rhs)?;

        // let user_data = input.user_data();

        // match value {
        //     Value::Function(ref f) => {
        //         let inherited = ident.link(user_data, Some(f.clone().consume_for_type()))?;
        //         if !inherited {
        //             unreachable!()
        //         }
        //     },
        //     Value::Ident(..) => {
        //         ident.link_from_pointed_type_with_lookup(user_data)?;
        //     }
        //     Value::Number(ref number) => {
        //         let ty = number.clone().into_type();
        //         ident.link(user_data, Some(ty))?;
        //     }
        // }


        // Ok(Assignment { ident, value })
    }
}
