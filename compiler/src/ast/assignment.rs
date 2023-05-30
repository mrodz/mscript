use std::borrow::Cow;

use anyhow::Result;

use crate::{
    ast::CompiledItem,
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{value::Value, Compile, Dependencies, Dependency, Ident};

#[derive(Debug, Clone)]
pub(crate) struct Assignment {
    pub ident: Ident,
    pub value: Value,
}

impl Dependencies for Assignment {
    fn supplies(&self) -> Vec<Dependency> {
        // let value = &self.value as &dyn Dependencies;
        // println!("\t\tIntroduced {}", self.ident.name());
        vec![Dependency::new(Cow::Borrowed(&self.ident))]
    }

    fn dependencies(&self) -> Vec<Dependency> {
        let value = &self.value as &dyn Dependencies;
        // println!("\tLooking up the dependencies for {}", self.ident.name());
        value.net_dependencies()
    }
}

impl Compile for Assignment {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        let name = &self.ident.name();

        let matched = match &self.value {
            Value::Ident(ident) => {
                vec![instruction!(load ident), instruction!(store name)]
            }
            Value::Function(function) => {
                let compiled_function = function.compile(function_buffer)?.remove(0);
                let CompiledItem::Function { ref id, ref location, .. } = compiled_function else {
                    unreachable!()
                };

                let dependencies = function.net_dependencies();

                let mut arguments = Vec::with_capacity(dependencies.len() + 1);

                let x = location.replace('\\', "/");

                arguments.push(format!("{x}#{}", id.to_string()));

                dependencies
                    .iter()
                    .map(|ident| ident.name().clone())
                    .collect_into(&mut arguments);

                let arguments = arguments.into_boxed_slice();

                // as per `bytecode/src/instruction_constants.rs`
                const MAKE_FUNCTION: u8 = 0x0D;

                let make_function_instruction = CompiledItem::Instruction {
                    id: MAKE_FUNCTION,
                    arguments,
                };

                vec![
                    // compiled_function,
                    make_function_instruction,
                    instruction!(store name),
                ]
            }
            Value::Number(number) => {
                let mut number_init = number.compile(function_buffer)?;

                number_init.push(instruction!(store name));

                number_init
            }
            Value::String(string) => {
                let mut string_init = string.compile(function_buffer)?;

                string_init.push(instruction!(store name));

                string_init
            }
            Value::MathExpr(math_expr) => {
                let mut string_init = math_expr.compile(function_buffer)?;

                string_init.push(instruction!(store name));

                string_init
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
            Rule::assignment_type => Self::assignment_type(child)?,
            _ => unreachable!(),
        };

        Ok(x)
    }
}
