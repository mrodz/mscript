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
mod math_expr;
mod number;
mod print_statement;
mod string;
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
use pest::Span;
pub(crate) use print_statement::PrintStatement;
pub(crate) use r#type::TypeLayout;
pub(crate) use value::Value;

use anyhow::{anyhow, bail, Context, Error, Result};
use bytecode::compilation_lookups::raw_byte_instruction_to_string_representation;
use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(Debug, Clone)]
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
        content: Option<Vec<CompiledItem>>,
        location: Rc<String>,
    },
    Instruction {
        id: u8,
        arguments: Box<[String]>,
    },
}

impl CompiledItem {
    pub fn repr(&self, use_string_version: bool) -> String {
        fn fix_arg_if_needed(arg: &String) -> Result<Cow<String>> {
            let starts = arg.starts_with('\"');
            let ends = arg.ends_with('\"');

            if starts ^ ends {
                bail!("non-matching `\"` on arg {arg}")
            }

            let has_quotes = starts && ends;

            if !has_quotes && arg.contains(' ') {
                Ok(Cow::Owned("\"".to_owned() + arg + "\""))
            } else {
                Ok(Cow::Borrowed(arg))
            }
        }

        match self {
            Self::Function { id, content, .. } => {
                let Some(ref content) = content else {
                    unreachable!();
                };

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
                        let arg = fix_arg_if_needed(arg).unwrap();
                        args.push_str(arg.as_str());
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
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>>;
}

pub(crate) trait Optimize {}

pub(crate) struct Dependency<'a> {
    pub ident: Cow<'a, Ident>,
}

impl PartialEq for Dependency<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ident.name() == other.ident.name()
    }
}

impl<'a> Dependency<'a> {
    pub fn name(&self) -> &String {
        self.ident.as_ref().name()
    }
    pub fn new(ident: Cow<'a, Ident>) -> Self {
        Self { ident }
    }
}

impl Debug for Dependency<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
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
    fn supplies(&self) -> Vec<Dependency> {
        vec![]
    }

    fn dependencies(&self) -> Vec<Dependency> {
        vec![]
    }

    fn net_dependencies(&self) -> Vec<Dependency> {
        let supplies = self.supplies();
        let dependencies = self.dependencies();

        dependencies
            .into_iter()
            .filter(|dependency| !supplies.contains(&dependency))
            .collect()
    }
}

pub fn new_err(span: Span, file_name: &str, message: String) -> Error {
    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(&file_name);

    anyhow!(custom_error)
}

pub fn map_err<R>(
    value: impl Into<Result<R>>,
    span: Span,
    file_name: &str,
    message: String,
) -> Result<R> {
    map_err_messages(value.into(), span, file_name, message, || Vec::<u8>::new())
}

pub fn map_err_messages<R, C>(
    mut value: Result<R>,
    span: Span,
    file_name: &str,
    message: String,
    description_messages: impl FnOnce() -> Vec<C>,
) -> Result<R>
where
    C: Display + Send + Sync + 'static,
{
    if value.is_ok() {
        return value; // no unwrap
    }

    let description_messages = description_messages();
    for message in description_messages {
        value = value.context(message)
    }

    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(&file_name);

    value.context(anyhow!(custom_error))
}
