mod assignment;
mod assignment_no_type;
mod assignment_type;
mod callable;
mod declaration;
mod function;
mod function_arguments;
mod function_body;
mod function_parameters;
mod function_return_type;
mod ident;
mod number;
mod print_statement;
mod r#type;
mod value;

pub(crate) use assignment::Assignment;
pub(crate) use callable::Callable;
pub(crate) use declaration::Declaration;
pub(crate) use function::Function;
pub(crate) use function_arguments::FunctionArguments;
pub(crate) use function_body::FunctionBody;
pub(crate) use function_parameters::FunctionParameters;
pub(crate) use ident::Ident;
pub(crate) use number::Number;
pub(crate) use print_statement::PrintStatement;
pub(crate) use r#type::TypeLayout;
pub(crate) use value::Value;

use anyhow::Result;
use bytecode::compilation_lookups::raw_byte_instruction_to_string_representation;
use std::{borrow::Cow, fmt::Display, rc::Rc};

#[derive(Debug)]
pub(crate) enum CompiledFunctionId {
    Generated(isize),
    Custom(String),
}

impl Display for CompiledFunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use function::name_from_function_id;
        match self {
            CompiledFunctionId::Custom(string) => write!(f, "{string}"),
            CompiledFunctionId::Generated(id) => write!(f, "{}", name_from_function_id(*id)),
        }
    }
}

#[derive(Debug)]
pub(crate) enum CompiledItem {
    Function {
        id: CompiledFunctionId,
        content: Vec<CompiledItem>,
        location: Rc<String>,
    },
    Instruction {
        id: u8,
        arguments: Box<[String]>,
    },
}

impl CompiledItem {
    pub fn repr(&self, use_string_version: bool) -> String {
        match self {
            Self::Function { id, content, .. } => {
                let content: String = content.iter().map(|x| x.repr(use_string_version)).collect();

                let func_name = match id {
                    CompiledFunctionId::Generated(id) => {
                        Cow::Owned(function::name_from_function_id(*id))
                    }
                    CompiledFunctionId::Custom(str) => Cow::Borrowed(str),
                };

                let (sep, f, e) = if use_string_version {
                    ('\n', "function", "end")
                } else {
                    ('\0', "f", "e")
                };

                format!("{f} {func_name}{sep}{content}{e}{sep}")
            }
            Self::Instruction { id, arguments } => {
                let mut args = String::new();

                if arguments.len() >= 1 {
                    for arg in &arguments[..] {
                        args.push(' ');
                        if arg.contains(' ') {
                            args.push('\"');
                            args.push_str(&arg);
                            args.push('\"');
                        } else {
                            args.push_str(&arg);
                        }
                    }
                }

                if use_string_version {
                    format!(
                        "\t{}{args}\n",
                        raw_byte_instruction_to_string_representation(*id).unwrap()
                    )
                } else {
                    format!("{}{}\0", *id as char, args)
                }
            }
        }
    }
}

#[macro_export]
macro_rules! instruction {
    ($name:tt $($arg:tt)*) => {{
        use crate::ast::*;

        let id = bytecode::compilation_lookups::string_instruction_representation_to_byte(stringify!($name))
            .expect("instruction does not exist");

        #[allow(unused_mut)]
        let mut arguments = vec![];

        $(
            arguments.push($arg.to_string());
        )*

        let arguments = arguments.into_boxed_slice();

        CompiledItem::Instruction { id: *id, arguments }

    }};
}

pub(crate) trait Compile {
    fn compile(&self) -> Result<Vec<CompiledItem>>;
}

pub(crate) trait Optimize {}

#[derive(Debug, PartialEq)]
pub(crate) struct Dependency<'a> {
    pub ident: Cow<'a, Ident>,
}

impl<'a> Dependency<'a> {
    pub fn name(&self) -> &String {
        self.ident.as_ref().name()
    }
    pub fn new(ident: Cow<'a, Ident>) -> Self {
        Self { ident }
    }
}

impl Display for Dependency<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl<'a> From<&'a Ident> for Dependency<'a> {
    fn from(value: &'a Ident) -> Self {
        Self {
            ident: Cow::Borrowed(value),
        }
    }
}

impl<'a> From<Ident> for Dependency<'a> {
    fn from(value: Ident) -> Self {
        Self {
            ident: Cow::Owned(value),
        }
    }
}

pub(crate) trait Dependencies {
    fn supplies(&self) -> Option<Box<[Dependency]>> {
        None
    }

    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        None
    }
}
