use anyhow::Result;

use crate::{
    ast::CompiledFunctionId,
    parser::{Node, Parser},
    scope::ScopeType,
};

use super::{
    r#type::IntoType, Compile, CompiledItem, Dependencies, Dependency, FunctionBody,
    FunctionParameters, TypeLayout,
};

pub static mut FUNCTION_ID: isize = 0;

pub fn name_from_function_id(id: isize) -> String {
    format!("__fn{id}")
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arguments: FunctionParameters,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: FunctionParameters,
}

impl IntoType for Function {
	/// unimplemented
	fn into_type(&self) -> TypeLayout {
		unimplemented!()
	}

    fn consume_for_type(self) -> TypeLayout
    where
        Self: Sized,
    {
		TypeLayout::Function(FunctionType {
            arguments: self.arguments,
        })
    }
}

impl Dependencies for Function {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        self.body.get_dependencies()
    }
}

impl Compile for Function {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let mut args = self.arguments.compile()?;
        let mut body = self.body.compile()?;

        args.append(&mut body);

        unsafe {
            let x = CompiledItem::Function {
                id: CompiledFunctionId::Generated(FUNCTION_ID),
                content: args,
            };
            FUNCTION_ID += 1;

            Ok(vec![x])
        }
    }
}

impl Parser {
    pub fn function(input: Node) -> Result<Function> {
        input.user_data().push_scope(ScopeType::Function);

        let mut children = input.children();
        let arguments = children.next().unwrap();
        let function_body = children.next().unwrap();

        let arguments = Self::function_parameters(arguments)?;
        let body = Self::function_body(function_body)?;

        input.user_data().pop_scope();

        Ok(Function { arguments, body })
    }
}
