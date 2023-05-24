use anyhow::Result;

use crate::{
    ast::CompiledFunctionId,
    parser::{Node, Parser, Rule},
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
    pub return_type: Option<&'static TypeLayout>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: FunctionParameters,
    pub return_type: Option<&'static TypeLayout>
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
            return_type: self.return_type
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
        input.user_data().run_in_scope(ScopeType::Function, || {
            let mut children = input.children();

            let arguments = children.next().unwrap();
            let arguments = Self::function_parameters(arguments)?;

            let next = children.next();

            // if there are no more children, there is no return type or body
            let Some(next) = next else {
                return Ok(Function { arguments, body: FunctionBody::empty_body(), return_type: None })
            };

            let (body, return_type) = if matches!(next.as_rule(), Rule::function_return_type) {
                let body = children.next();
                (body, Some(next))
            } else {
                (Some(next), None)
            };

            let body = if let Some(body) = body {
                Self::function_body(body)?
            } else {
                FunctionBody::empty_body()
            };

            let return_type = if let Some(return_type) = return_type {
                Some(Self::function_return_type(return_type)?)
            } else {
                None
            };

            Ok(Function { arguments, body, return_type })
        })
    }
}
