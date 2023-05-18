mod assignment;
mod callable;
mod function;
mod function_arguments;
mod function_body;
mod function_parameters;
mod ident;
mod number;
mod declaration;
mod value;

pub use assignment::Assignment;
pub use callable::Callable;
pub use function::Function;
pub use function_arguments::FunctionArguments;
pub use function_body::FunctionBody;
pub use function_parameters::FunctionParameters;
pub use ident::Ident;
pub use number::Number;
pub use declaration::Declaration;
pub use value::Value;

pub(crate) trait Compile {}

pub(crate) trait Optimize {}

pub(crate) trait Dependencies {
    fn get_dependencies(&self) -> Option<Box<[&Ident]>> {
        None
    }
}
