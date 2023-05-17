mod assignment;
mod function;
mod function_arguments;
mod function_body;
mod ident;
mod statement;
mod top_level_declaration;
mod value;

pub use assignment::Assignment;
pub use function::Function;
pub use function_arguments::FunctionArguments;
pub use function_body::FunctionBody;
pub use ident::Ident;
pub use statement::Statement;
pub use top_level_declaration::TopLevelDeclaration;
pub use value::Value;

pub(crate) trait Compile {

}

pub(crate) trait Optimize {
	
}