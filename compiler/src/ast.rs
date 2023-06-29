mod assignment;
mod assignment_no_type;
mod assignment_type;
mod boolean;
mod callable;
mod declaration;
mod function;
mod function_arguments;
mod function_body;
mod function_parameters;
mod function_return_type;
mod ident;
mod if_statement;
mod math_expr;
mod number;
mod print_statement;
mod r#return;
mod string;
mod r#type;
mod value;

pub(crate) use assignment::Assignment;
pub(crate) use callable::Callable;
pub(crate) use declaration::Declaration;
pub(crate) use function::Function;
pub(crate) use function_arguments::FunctionArguments;
pub(crate) use function_body::Block;
pub(crate) use function_parameters::FunctionParameters;
pub(crate) use ident::Ident;
pub(crate) use number::Number;
pub(crate) use print_statement::PrintStatement;
pub(crate) use r#type::TypeLayout;
pub(crate) use value::Value;

use anyhow::{anyhow, bail, Context, Error, Result};
use bytecode::compilation_lookups::raw_byte_instruction_to_string_representation;
use pest::Span;
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
    pub fn repr(&self, use_string_version: bool) -> Result<String> {
        fn fix_arg_if_needed(arg: &String) -> Result<Cow<String>> {
            let starts = arg.starts_with('\"');
            let ends = arg.ends_with('\"');

            if starts ^ ends {
                bail!("non-matching `\"` on arg {arg}")
            }

            let has_quotes = starts && ends;

            if !has_quotes && arg.contains(' ') {
                let mut combined = String::with_capacity(arg.len() + 2);
                combined.push('"');
                combined.push_str(arg);
                combined.push('"');
                Ok(Cow::Owned(combined))
            } else {
                Ok(Cow::Borrowed(arg))
            }
        }

        match self {
            Self::Function { id, content, .. } => {
                let Some(ref content) = content else {
                    bail!("this is a function symbol, not a compilable function. {self:?}");
                };

                let mut result = String::new();
                for item in content {
                    result += &item.repr(use_string_version)?;
                }

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

                Ok(format!("{f} {func_name}{sep}{result}{e}{sep}"))
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
                    Ok(format!(
                        "\t{}{args}\n",
                        raw_byte_instruction_to_string_representation(*id).unwrap()
                    ))
                } else {
                    Ok(format!("{}{}\0", *id as char, args))
                }
            }
        }
    }
}

#[macro_export]
macro_rules! instruction {
    ($name:tt $($arg:tt)*) => {{
        use $crate::ast::*;

        let id = bytecode::compilation_lookups::string_instruction_representation_to_byte(stringify!($name))
            .expect("instruction does not exist");

        let arguments = vec![
            $(
                $arg.to_string(),
            )*
        ];

        let arguments = arguments.into_boxed_slice();

        CompiledItem::Instruction { id: *id, arguments }

    }};
}

pub(crate) trait Compile {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>>;
}

pub(crate) trait Optimize {}

#[derive(Clone, Debug)]
pub(crate) struct Dependency<'a> {
    pub ident: Cow<'a, Ident>,
    pub cycles_needed: usize
}

impl PartialEq for Dependency<'_> {
    fn eq(&self, other: &Self) -> bool {
        let same_names: bool = self.ident.name() == other.ident.name();
        let same_types: bool = self.ident.ty().unwrap() == other.ident.ty().unwrap();

        same_names && same_types 
    }
}

impl<'a> Dependency<'a> {
    pub fn name(&self) -> &String {
        self.ident.as_ref().name()
    }
    pub fn new(ident: Cow<'a, Ident>) -> Self {
        Self { ident, cycles_needed: 0 }
    }
    pub fn increment_cycle(&mut self) {
        self.cycles_needed += 1;
    }

    /// For self = `Dependency(x)` and other = `Dependency(y)`, check if `x == y` or `x == CallbackVariable(y)`
    /// 
    /// # Errors
    /// Will error if either dependency is typeless.
    pub fn eq_allow_callbacks(&self, other: &Self) -> Result<bool> {

        if self.ident.name() != other.ident.name() {
            return Ok(false);
        }

        let other_ty: &TypeLayout = other.ident.ty()?.as_ref();
        let self_ty: &TypeLayout = self.ident.ty()?.as_ref();

        if let TypeLayout::CallbackVariable(ptr_ty) = other_ty {
            if other.cycles_needed > 0 {
                return Ok(self_ty == ptr_ty.as_ref()) 
            }
        } 

        Ok(self_ty == other_ty)
    }
}

impl Display for Dependency<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl<'a> From<&'a Ident> for Dependency<'a> {
    fn from(value: &'a Ident) -> Self {
        Self::new(Cow::Borrowed(value))
    }
}

/// Default behavior for [`Dependencies::net_dependencies`]. This function is only
/// separate for public visibility, which allows implementations to call it.
/// 
/// Formula:
/// `[dependencies] - [supplies] = [net]`
/// 
/// The algorithm for "satisfying" dependencies is as follows:
/// * If dependency.name != supplied.name, continue
/// * If dependency is a variable from a child scope, succeed if supplied.type == dependency.type
/// * If dependency is not a variable from a child scope, it will be propagated regardless of whether the types 
///   match (required for cases where a variable from a parent scope is copied to a separate local variable with the same name).
/// * Succeed if supplied.type == dependency.type
pub(crate) fn get_net_dependencies(ast_item: &dyn Dependencies, is_scope: bool) -> Vec<Dependency> {
    let supplies = ast_item.supplies();
    let dependencies = ast_item.dependencies();

    let mut result: Vec<Dependency> = Vec::with_capacity(dependencies.len());

    // For now, this is O(n^2) :(
    'dependency_loop: for mut dependency in dependencies {
        for supplied in &supplies {
            if supplied.eq_allow_callbacks(&dependency).expect("idents do not have types") {
                continue 'dependency_loop;
            }
        }

        if is_scope {
            dependency.increment_cycle();
        }

        result.push(dependency);
    }

    result
}

pub(crate) trait Dependencies {
    /// This method expresses all new variables and identities created by an AST node.
    /// Identities supplied by a parent can be consumed by child AST nodes, but not the
    /// other way around.
    /// 
    /// # Example
    /// A function might have a parameter `input`, and define variables `sum` and `product`.
    /// Thus, a function's [`Dependencies::supplies`] implementation should return `vec![input, sum, product]`. 
    fn supplies(&self) -> Vec<Dependency> {
        vec![]
    }

    /// This method expresses all outstanding variables needed by this function. Generally, 
    /// if an AST node comes across an identity, it should add it as a dependency. This even
    /// applies to variables defined inside a scope--If you see it, add it as a dependency.
    /// 
    /// # Example
    /// A function might have a parameter `input`, define variables `sum` and `product`, and
    /// returns `input * product + sum`. Thus, a function's [`Dependencies::dependencies`] implementation
    /// should return `vec![input, sum, product]`.
    fn dependencies(&self) -> Vec<Dependency> {
        vec![]
    }

    /// This function is used to calculate the outstanding dependencies by filtering out identities
    /// _needed_ from the identities _supplied_ by an AST node. If implementing an AST node that
    /// **DIRECTLY** creates a scope (For example, a block of code), this method should be overriden. 
    /// 
    /// [`get_net_dependencies(self, false)`](get_net_dependencies) is the default implementation.
    /// Override this function and pass `true` if dealing with an AST node that creates a scope.
    fn net_dependencies(&self) -> Vec<Dependency>
    where
        Self: Sized
    {
        get_net_dependencies(self, false)
    }
}

pub fn new_err(span: Span, file_name: &str, message: String) -> Error {
    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(file_name);

    anyhow!(custom_error)
}

pub fn map_err<R>(
    value: impl Into<Result<R>>,
    span: Span,
    file_name: &str,
    message: String,
) -> Result<R> {
    map_err_messages(value.into(), span, file_name, message, Vec::<u8>::new)
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
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(file_name);

    value.context(anyhow!(custom_error))
}
