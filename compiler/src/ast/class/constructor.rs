use std::{borrow::Cow, sync::Arc, path::PathBuf};

use anyhow::Result;
use bytecode::compilation_bridge::id::{MAKE_FUNCTION, RET};

use crate::{
    ast::{
        function::FunctionType, r#type::IntoType, Block, CompilationState, Compile,
        CompiledFunctionId, CompiledItem, Dependencies, FunctionParameters, Ident, TypeLayout,
    },
    instruction,
    parser::{Node, Parser},
    scope::ScopeReturnStatus,
    VecErr, BytecodePathStr,
};

use super::WalkForType;

#[derive(Debug)]
pub struct Constructor {
    parameters: FunctionParameters,
    body: Block,
    path_str: Arc<PathBuf>,
    class_name: Arc<String>,
}

impl Constructor {
    pub fn symbolic_id(&self) -> String {
        format!("{}::$constructor", self.class_name)
    }

    pub fn default_constructor(
        path_str: Arc<PathBuf>,
        class_name: Arc<String>,
    ) -> Self {
        Self {
            parameters: FunctionParameters::Named(vec![Ident::new(
                "self".to_owned(),
                Some(Cow::Owned(TypeLayout::ClassSelf)),
                true,
            )]),
            body: Block::empty_body(),
            path_str,
            class_name,
        }
    }
}

impl Compile for Constructor {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let symbolic_id = self.symbolic_id();
        let mut args = self.parameters.compile(state)?;
        // args.push(instruction!(breakpoint));
        let mut body = self.body.compile(state)?;

        if let Some(CompiledItem::Instruction { id: RET, .. }) = body.last() {
            // nothing! the function returns by itself
        } else {
            body.push(instruction!(void));
            body.push(instruction!(ret));
        }

        args.append(&mut body);

        let id = CompiledFunctionId::Custom(symbolic_id);

        let real_function = CompiledItem::Function {
            content: Some(args),
            location: self.path_str.clone(),
            id: id.clone(),
        };

        state.push_function(real_function);

        let dependencies = self.net_dependencies();

        let mut arguments = Vec::with_capacity(dependencies.len() + 1);

        let x = self.path_str.bytecode_str();

        arguments.push(format!("{x}#{id}"));

        for dependency in dependencies {
            arguments.push(dependency.name().clone());
        }

        let arguments = arguments.into_boxed_slice();

        let make_function_instruction = CompiledItem::Instruction {
            id: MAKE_FUNCTION,
            arguments,
        };

        let constructor_register = state.poll_temporary_register();
        let obj_register = state.poll_temporary_register();
        let mut result = vec![
            instruction!(make_object),
            instruction!(store_fast obj_register),
            make_function_instruction,
            instruction!(store_fast constructor_register),
            instruction!(load_fast obj_register),
        ];

        for i in 0..self.parameters.len() - 1 {
            result.push(instruction!(arg i))
        }

        result.extend_from_slice(&[
            instruction!(load_fast constructor_register),
            // instruction!(breakpoint),
            instruction!(call),
            instruction!(delete_name_scoped constructor_register),
            instruction!(load_fast obj_register),
        ]);

        Ok(result)
    }
}

impl WalkForType for Constructor {
    fn type_from_node(input: &Node) -> Result<Ident> {
        let parameters = input.children().next().unwrap();
        let parameters = Parser::function_parameters(parameters, false, true, true)?;

        let function_type = FunctionType::new(Arc::new(parameters), ScopeReturnStatus::Void);

        let ident = Ident::new(
            "$constructor".to_owned(),
            Some(Cow::Owned(TypeLayout::Function(function_type))),
            true,
        );

        Ok(ident)
    }
}

impl IntoType for Constructor {
    fn for_type(&self) -> Result<crate::ast::TypeLayout> {
        let function_type =
            FunctionType::new(Arc::new(self.parameters.clone()), ScopeReturnStatus::Void);

        Ok(TypeLayout::Function(function_type))
    }
}

impl Dependencies for Constructor {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        self.body.net_dependencies()
    }

    fn supplies(&self) -> Vec<crate::ast::Dependency> {
        self.parameters.supplies()
    }
}

impl Parser {
    pub fn constructor(input: Node) -> Result<Constructor, Vec<anyhow::Error>> {
        let mut children = input.children();
        let parameters = children.next().unwrap();

        // We need to keep the handle alive so that it is dropped at the end of this scope.
        // Using "_" or not saving it as a variable causes the scope to pop instantly.
        let _scope_handle = input.user_data().push_function(ScopeReturnStatus::Void);

        let parameters = Self::function_parameters(parameters, true, true, true).to_err_vec()?;

        let body = children.next().unwrap();
        let body = Self::block(body)?;

        let class_type = input
            .user_data()
            .get_owned_type_of_executing_class()
            .unwrap();

        let path_str = input.user_data().bytecode_path();

        Ok(Constructor {
            parameters,
            body,
            path_str,
            class_name: class_type.arced_name(),
        })
    }
}
