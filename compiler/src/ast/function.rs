use std::{
    borrow::Cow,
    cell::{Ref, RefCell},
    collections::HashSet,
    fmt::Display,
    hash::Hash,
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};

use anyhow::{anyhow, Result};
use bytecode::compilation_bridge::id::{MAKE_FUNCTION, RET};

use crate::{
    ast::new_err,
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    BytecodePathStr, VecErr,
};

use super::{
    r#type::{IntoType, TypecheckFlags},
    Block, ClassType, CompilationState, Compile, CompiledItem, Dependencies, Dependency,
    FunctionParameters, TypeLayout,
};

#[derive(Debug)]
pub(crate) struct Function {
    pub parameters: Rc<FunctionParameters>,
    pub body: Block,
    pub return_type: ScopeReturnStatus,
    pub path_str: Arc<PathBuf>,
    is_associated_fn: bool,
    is_constructor: bool,
}

#[derive(Debug, Clone, Eq)]
pub(crate) struct FunctionType {
    parameters: Rc<FunctionParameters>,
    return_type: Box<RefCell<ScopeReturnStatus>>,
    is_associated_fn: bool,
    is_constructor: bool,
}

impl FunctionType {
    pub fn return_type(&self) -> Ref<ScopeReturnStatus> {
        self.return_type.borrow()
    }

    pub const fn is_constructor(&self) -> bool {
        self.is_constructor
    }

    pub const fn is_associated_fn(&self) -> bool {
        self.is_associated_fn
    }

    pub(crate) fn set_return_type(&mut self, new_status: ScopeReturnStatus) {
        *self.return_type.get_mut() = new_status;
    }

    pub(crate) fn try_set_return_type(&self, new_status: ScopeReturnStatus) {
        *self.return_type.borrow_mut() = new_status;
    }

    pub fn parameters(&self) -> &FunctionParameters {
        &self.parameters
    }

    pub fn parameters_mut(&mut self) -> &mut Rc<FunctionParameters> {
        &mut self.parameters
    }
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len() {
            return false;
        }

        let return_types = self
            .return_type
            .borrow()
            .eq_for_signature_checking(&other.return_type.borrow());

        if let Ok(false) | Err(..) = return_types {
            return false;
        }

        let t1 = self.parameters.to_types();
        let t2 = other.parameters.to_types();

        if t1.len() != t2.len() {
            return false;
        }

        t1.iter()
            .zip(t2.iter())
            .all(|(x, y)| x.eq_complex(y, &TypecheckFlags::<&ClassType>::signature_check()))
    }
}

impl Hash for FunctionType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parameters.to_types().hash(state);

        if let Some(return_type) = self.return_type.borrow().get_type() {
            return_type.hash(state);
        };
    }
}

#[cfg(test)]
pub mod eq_hash_test {
    use crate::{
        assert_proper_eq_hash,
        ast::{r#type::NativeType, *},
    };

    use super::*;

    #[test]
    fn names_names() {
        let lhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "a".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );
        let rhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "a".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    fn types_names() {
        let lhs = FunctionType::new(
            Rc::new(FunctionParameters::TypesOnly(vec![Cow::Owned(
                TypeLayout::Native(NativeType::Int),
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );
        let rhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "a".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    fn different_names_same_types() {
        let lhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "a".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );
        let rhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "b".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Void,
            false,
            false,
        );

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    fn return_types_at_different_stages() {
        let lhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "a".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Bool))),
            false,
            false,
        );
        let rhs = FunctionType::new(
            Rc::new(FunctionParameters::Named(vec![Ident::new(
                "b".into(),
                Some(Cow::Owned(TypeLayout::Native(NativeType::Int))),
                false,
            )])),
            ScopeReturnStatus::Did(Cow::Owned(TypeLayout::Native(NativeType::Bool))),
            false,
            false,
        );

        assert_proper_eq_hash!(lhs, rhs);
    }
}

impl FunctionType {
    pub fn new(
        parameters: Rc<FunctionParameters>,
        return_type: ScopeReturnStatus,
        is_associated_fn: bool,
        is_constructor: bool,
    ) -> Self {
        Self {
            parameters,
            return_type: Box::new(RefCell::new(return_type)),
            is_associated_fn,
            is_constructor,
        }
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_constructor() {
            return write!(
                f,
                "{}",
                self.return_type()
                    .get_type()
                    .expect("constructor does not return a type")
            );
        }

        let return_type: Cow<'static, str> = if let ScopeReturnStatus::Should(return_type)
        | ScopeReturnStatus::Did(return_type) =
            &*self.return_type.borrow()
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
    pub const fn new(
        parameters: Rc<FunctionParameters>,
        body: Block,
        return_type: ScopeReturnStatus,
        path_str: Arc<PathBuf>,
        is_associated_fn: bool,
        is_constructor: bool,
    ) -> Self {
        Self {
            parameters,
            body,
            return_type,
            path_str,
            is_associated_fn,
            is_constructor,
        }
    }
}

impl IntoType for Function {
    fn for_type(&self) -> Result<TypeLayout> {
        Ok(TypeLayout::Function(FunctionType::new(
            self.parameters.clone(),
            self.return_type.clone(),
            self.is_associated_fn,
            self.is_constructor,
        )))
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
        state: &CompilationState,
    ) -> Result<Vec<CompiledItem>> {
        // adds the real function to the function buffer, and returns a shadow function without a body.
        let shadow_function = self.compile(state)?.remove(0);

        let CompiledItem::Function { id, location, .. } = shadow_function else {
            unreachable!()
        };

        let dependencies = self.net_dependencies();

        let mut dependency_list = HashSet::with_capacity(dependencies.len() + 1);

        let x = location.bytecode_str();

        for dependency in dependencies {
            dependency_list.insert(dependency.name().to_owned());
        }

        let mut arguments = vec![format!("{x}#{id}")];
        arguments.extend(dependency_list);
        let arguments = arguments.into_boxed_slice();

        let make_function_instruction = CompiledItem::Instruction {
            id: MAKE_FUNCTION,
            arguments,
        };

        Ok(vec![make_function_instruction])
    }
}

impl Compile for Function {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>> {
        let mut args = self.parameters.compile(state)?;
        let mut body = self.body.compile(state)?;

        if let Some(CompiledItem::Instruction { id: RET, .. }) = body.last() {
            // nothing! the function returns by itself
        } else {
            body.push(instruction!(void));
            body.push(instruction!(ret));
        }

        args.append(&mut body);

        let id = state.poll_function_id();

        let x = CompiledItem::Function {
            id: id.clone(),
            content: Some(args),
            location: self.path_str.clone(),
        };

        state.push_function(x);

        Ok(vec![CompiledItem::Function {
            id,
            content: None,
            location: self.path_str.clone(),
        }])
    }
}

impl Parser {
    pub fn function(input: Node) -> Result<Function, Vec<anyhow::Error>> {
        let path_str = input.user_data().bytecode_path();

        let mut children = input.children();
        let parameters = children.next().unwrap();

        let next = children.next();

        // if there are no more children, there is no return type or body
        let Some(next) = next else {
            // We are reading parameters without pushing the function frame,
            // so the parameters MUST never be used. Essentially, we are just
            // syntax checks.
            let parameters = Self::function_parameters(parameters, false, false, false)
                .map_err(|e| vec![anyhow!(e)])?;

            return Ok(Function::new(
                Rc::new(parameters),
                Block::empty_body(),
                ScopeReturnStatus::Void,
                path_str,
                false,
                false,
            ));
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

        let parameters =
            Rc::new(Self::function_parameters(parameters, true, false, false).to_err_vec()?);

        let maybe_previous = input
            .user_data()
            .register_function_parameters_to_scope(parameters.clone());
        assert!(
            maybe_previous.is_none(),
            "The function's parameters have already been set: found `{maybe_previous:?}`"
        );

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

        Ok(Function::new(
            parameters,
            body,
            return_type,
            path_str,
            false,
            false,
        ))
    }
}
