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
    pub flags: Option<Box<[AssignmentFlags]>>
}

impl Assignment {
    pub fn new(ident: Ident, value: Value) -> Self {
        Self {
            ident,
            value,
            flags: None
        }
    }

    pub fn set_flags(&mut self, flags: Box<[AssignmentFlags]>) {
        self.flags = Some(flags);
    }
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
        let name = self.ident.name();

        let mut value_init = match &self.value {
            Value::Ident(ident) => {
                vec![instruction!(load ident)]
            }
            Value::Function(function) => {
                function.in_place_compile_for_value(function_buffer)?
            }
            Value::Number(number) => {
                number.compile(function_buffer)?
            }
            Value::String(string) => {
                string.compile(function_buffer)?
            }
            Value::MathExpr(math_expr) => {
                math_expr.compile(function_buffer)?
            }
            Value::Callable(callable) => {
                callable.compile(function_buffer)?
            }
            Value::Boolean(boolean) => {
                boolean.compile(function_buffer)?
            }
        };

        let store_instruction = if let Some(ref flags) = self.flags {
            if flags.contains(&AssignmentFlags::Lookup) {
                instruction!(store_object name)
            } else {
                instruction!(store name)
            }
        } else {
            instruction!(store name)
        };

        value_init.push(store_instruction);

        Ok(value_init)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentFlags {
    Lookup
}

impl From<&str> for AssignmentFlags {
    fn from(value: &str) -> Self {
        match value {
            "lookup" => Self::Lookup,
            other => unimplemented!("{other}")
        }
    }
}

impl Parser {
    pub fn assignment_flags(input: Node) -> Result<Box<[AssignmentFlags]>> {
        let flags = input.children();

        let mut result = vec![];

        for flag in flags {
            result.push(flag.as_str().into());
        }

        Ok(result.into_boxed_slice())
    }

    pub fn assignment(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let maybe_flags_or_assignment = children.next().unwrap();

        let (flags, assignment) = if maybe_flags_or_assignment.as_rule() == Rule::assignment_flags {
            
            (Some(Self::assignment_flags(maybe_flags_or_assignment)?), children.next().unwrap())
        } else {
            (None, maybe_flags_or_assignment)
        };

        let mut x = match assignment.as_rule() {
            Rule::assignment_no_type => Self::assignment_no_type(assignment)?,
            Rule::assignment_type => Self::assignment_type(assignment)?,
            rule => unreachable!("{rule:?}"),
        };

        if let Some(flags) = flags {
            x.set_flags(flags)
        }

        Ok(x)
    }
}
