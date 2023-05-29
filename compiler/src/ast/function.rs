use std::{borrow::Cow, fmt::Display, rc::Rc};

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
pub(crate) struct Function {
    pub parameters: FunctionParameters,
    pub body: FunctionBody,
    pub return_type: Option<Cow<'static, TypeLayout>>,
    pub path_str: Rc<String>,
}

#[derive(Debug, Clone, Eq)]
pub(crate) struct FunctionType {
    pub parameters: FunctionParameters,
    pub return_type: ReturnType,
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len() {
            return false;
        }

        if self.return_type != other.return_type {
            return false;
        }

        let t1 = self.parameters.to_types();
        let t2 = other.parameters.to_types();

        t1 == t2
    }
}

impl FunctionType {
    pub fn new(parameters: FunctionParameters, return_type: ReturnType) -> Self {
        Self {
            parameters,
            return_type,
        }
    }
}

type ReturnType = Option<Box<Cow<'static, TypeLayout>>>;

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let return_type: Cow<'static, str> = if let Some(box return_type) = &self.return_type {
            let actual_type = match return_type.as_ref() {
                TypeLayout::Function(..) => {
                    format!("({})", return_type)
                }
                _ => return_type.to_string(),
            };

            Cow::Owned(" -> ".to_owned() + &actual_type)
        } else {
            Cow::Borrowed("")
        };

        write!(f, "fn({}){}", self.parameters, return_type)
    }
}

impl Function {
    pub fn new(
        parameters: FunctionParameters,
        body: FunctionBody,
        return_type: Option<Cow<'static, TypeLayout>>,
        path_str: Rc<String>,
    ) -> Self {
        Self {
            parameters,
            body,
            return_type,
            path_str,
        }
    }
}

impl IntoType for Function {
    /// unimplemented
    fn into_type(&self) -> Result<TypeLayout> {
        unimplemented!()
    }

    fn consume_for_type(self) -> Result<TypeLayout>
    where
        Self: Sized,
    {
        Ok(TypeLayout::Function(FunctionType::new(
            self.parameters,
            self.return_type.map(|x| Box::new(x)),
        )))
    }
}

impl Dependencies for Function {
    fn get_dependencies(&self) -> Option<Box<[Dependency]>> {
        let vars_used_by_body = self.body.get_dependencies();
        let vars_provided_by_parameters = self.parameters.supplies();

        let Some(body_vars) = vars_used_by_body else {
            return None;
        };

        let Some(param_vars) = vars_provided_by_parameters else {
            return Some(body_vars)
        };

        let body_vars = body_vars.into_vec();

        let x = body_vars
            .into_iter()
            .filter(|x| !param_vars.contains(x))
            .collect();
        Some(x)
    }
}

impl Compile for Function {
    fn compile(&self) -> Result<Vec<super::CompiledItem>> {
        let mut args = self.parameters.compile()?;
        let mut body = self.body.compile()?;

        args.append(&mut body);

        unsafe {
            let x = CompiledItem::Function {
                id: CompiledFunctionId::Generated(FUNCTION_ID),
                content: args,
                location: self.path_str.clone(),
            };
            FUNCTION_ID += 1;

            Ok(vec![x])
        }
    }
}

impl Parser {
    pub fn function(input: Node) -> Result<Function> {
        let path_str = input.user_data().get_source_file_name();
        input.user_data().run_in_scope(ScopeType::Function, || {
            let mut children = input.children();

            let parameters = children.next().unwrap();
            let parameters = Self::function_parameters(parameters)?;

            let next = children.next();

            // if there are no more children, there is no return type or body
            let Some(next) = next else {
                return Ok(Function::new(parameters, FunctionBody::empty_body(), None, path_str));
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

            Ok(Function::new(parameters, body, return_type, path_str))
        })
    }
}
