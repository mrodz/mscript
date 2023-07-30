use std::{borrow::Cow, fmt::Display, sync::Arc};

use anyhow::{anyhow, Result};

use crate::{
    ast::{new_err, CompiledFunctionId},
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::{
    r#type::IntoType, Block, Compile, CompiledItem, Dependencies, Dependency, FunctionParameters,
    TypeLayout,
};

pub static mut FUNCTION_ID: isize = 0;

pub fn name_from_function_id(id: isize) -> String {
    format!("__fn{id}")
}

#[derive(Debug)]
pub(crate) struct Function {
    pub parameters: Arc<FunctionParameters>,
    pub body: Block,
    pub return_type: ScopeReturnStatus,
    pub path_str: Arc<String>,
}

#[derive(Debug, Clone, Eq)]
pub(crate) struct FunctionType {
    parameters: Arc<FunctionParameters>,
    return_type: Box<ScopeReturnStatus>,
}

impl FunctionType {
    pub fn return_type(&self) -> &ScopeReturnStatus {
        &self.return_type
    }

    pub fn parameters(&self) -> &FunctionParameters {
        &self.parameters
    }
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len() {
            return false;
        }

        let return_types = self
            .return_type
            .eq_for_signature_checking(&other.return_type);

        if let Ok(false) | Err(..) = return_types {
            return false;
        }

        let t1 = self.parameters.to_types();
        let t2 = other.parameters.to_types();

        t1 == t2
    }
}

impl FunctionType {
    pub fn new(parameters: Arc<FunctionParameters>, return_type: ScopeReturnStatus) -> Self {
        Self {
            parameters,
            return_type: Box::new(return_type),
        }
    }
}

// type ReturnType = Option<Box<Cow<'static, TypeLayout>>>;

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let return_type: Cow<'static, str> = if let ScopeReturnStatus::Should(return_type)
        | ScopeReturnStatus::Did(return_type) =
            &self.return_type.as_ref()
        {
            let actual_type = match return_type.as_ref() {
                TypeLayout::Function(..) => {
                    format!("({return_type})")
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
        parameters: Arc<FunctionParameters>,
        body: Block,
        return_type: ScopeReturnStatus,
        path_str: Arc<String>,
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
    fn for_type(&self) -> Result<TypeLayout> {
        Ok(TypeLayout::Function(Arc::new(FunctionType::new(
            self.parameters.clone(),
            self.return_type.clone(),
        ))))
    }
}

impl Dependencies for Function {
    fn supplies(&self) -> Vec<Dependency> {
        self.parameters.supplies()
    }

    fn dependencies(&self) -> Vec<Dependency> {
        self.body.net_dependencies()
    }
}

impl Function {
    pub fn in_place_compile_for_value(
        &self,
        function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>> {
        // adds the real function to the function buffer, and returns a shadow function without a body.
        let shadow_function = self.compile(function_buffer)?.remove(0);

        let CompiledItem::Function { id, location, .. } = shadow_function else {
            unreachable!()
        };

        let dependencies = self.net_dependencies();

        let mut arguments = Vec::with_capacity(dependencies.len() + 1);

        let x = location.replace('\\', "/");

        arguments.push(format!("{x}#{id}"));

        for dependency in dependencies {
            arguments.push(dependency.name().clone());
        }

        let arguments = arguments.into_boxed_slice();

        // as per `bytecode/src/instruction_constants.rs`
        const MAKE_FUNCTION: u8 = 0x0D;

        let make_function_instruction = CompiledItem::Instruction {
            id: MAKE_FUNCTION,
            arguments,
        };

        Ok(vec![make_function_instruction])
    }
}

impl Compile for Function {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut args = self.parameters.compile(function_buffer)?;
        let mut body = self.body.compile(function_buffer)?;

        if let Some(CompiledItem::Instruction { id: RET, .. }) = body.last() {
            // nothing! the function returns by itself
        } else {
            body.push(instruction!(void));
            body.push(instruction!(ret));
        }

        args.append(&mut body);

        const RET: u8 = 0x12;

        unsafe {
            let id = CompiledFunctionId::Generated(FUNCTION_ID);
            let x = CompiledItem::Function {
                id: id.clone(),
                content: Some(args),
                location: self.path_str.clone(),
            };

            FUNCTION_ID += 1;

            function_buffer.push(x);

            Ok(vec![CompiledItem::Function {
                id,
                content: None,
                location: self.path_str.clone(),
            }])
            // Ok(vec![x])
        }
    }
}

impl Parser {
    pub fn function(input: Node) -> Result<Function, Vec<anyhow::Error>> {
        let path_str = input.user_data().get_file_name();

        #[cfg(feature = "debug")]
        let input_span = input.as_str();

        let mut children = input.children();
        let parameters = children.next().unwrap();

        // let parameters = Self::function_parameters(parameters)?;

        let next = children.next();

        // if there are no more children, there is no return type or body
        let Some(next) = next else {
            // We are reading parameters without pushing the function frame,
            // so the parameters MUST never be used. Essentially, we are just
            // syntax checks.
            let parameters = Self::function_parameters(parameters, false).map_err(|e| vec![anyhow!(e)])?;

            return Ok(Function::new(Arc::new(parameters), Block::empty_body(), ScopeReturnStatus::Void, path_str));
        };

        let (body, return_type) = if matches!(next.as_rule(), Rule::function_return_type) {
            let body = children.next();
            (body, Some(next))
        } else {
            (Some(next), None)
        };

        let return_type = if let Some(return_type) = return_type {
            Some(Self::function_return_type(return_type).to_err_vec()?)
        } else {
            None
        };

        let function_scope = input
            .user_data()
            .push_function(ScopeReturnStatus::detect_should_return(return_type));

        #[cfg(feature = "debug")]
        println!("\t^^ {input_span}");

        let parameters = Arc::new(Self::function_parameters(parameters, true).to_err_vec()?);

        // input.user_data().register_function_parameters_to_scope(Arc::clone(&parameters));

        let body = if let Some(body) = body {
            Self::block(body)?
        } else {
            Block::empty_body()
        };

        if !input.user_data().did_scope_exit_with_value_if_required() {
            return Err(vec![new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "this function reached its end without a return, when it expected a value"
                    .to_owned(),
            )]);
        }

        let return_type = function_scope.consume();

        Ok(Function::new(parameters, body, return_type, path_str))
    }
}
