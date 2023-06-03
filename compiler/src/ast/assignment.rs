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
                let mut function_init = function.in_place_compile_for_value(function_buffer)?;

                function_init.push(instruction!(store name));

                function_init
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
            Value::Callable(callable) => {
                let mut callable_init = callable.compile(function_buffer)?;

                callable_init.push(instruction!(store name));

                callable_init
            }
            Value::Boolean(boolean) => {
                let mut boolean_init = boolean.compile(function_buffer)?;

                boolean_init.push(instruction!(store name));

                boolean_init
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
