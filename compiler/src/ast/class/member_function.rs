use std::{borrow::Cow, sync::Arc};

use anyhow::Result;
use bytecode::compilation_bridge::id::{MAKE_FUNCTION, RET};

use crate::{
    ast::{
        function::FunctionType, Block, CompilationState, Compile, CompiledFunctionId, CompiledItem,
        Dependencies, FunctionParameters, Ident, TypeLayout,
    },
    instruction,
    parser::{Node, Parser, Rule},
    scope::ScopeReturnStatus,
    VecErr,
};

use super::WalkForType;

#[derive(Debug)]
pub(crate) struct MemberFunction {
    ident: Ident,
    parameters: FunctionParameters,
    body: Block,
    path_str: Arc<String>,
    class_name: Arc<String>,
    class_id: usize,
}

impl MemberFunction {
    pub fn symbolic_id(&self) -> String {
        format!(
            "{}_{}::{}",
            self.class_name,
            self.class_id,
            self.ident().name()
        )
    }
}

impl Compile for MemberFunction {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let mut args = self.parameters.compile(state)?;
        let mut body = self.body.compile(state)?;

        if let Some(CompiledItem::Instruction { id: RET, .. }) = body.last() {
            // nothing! the function returns by itself
        } else {
            body.push(instruction!(void));
            body.push(instruction!(ret));
        }

        args.append(&mut body);

        let id = CompiledFunctionId::Custom(self.symbolic_id());

        let x = CompiledItem::Function {
            id: id.clone(),
            content: Some(args),
            location: self.path_str.clone(),
        };

        state.push_function(x);

        let dependencies = self.net_dependencies();

        let mut arguments = Vec::with_capacity(dependencies.len() + 1);

        let x = self.path_str.replace('\\', "/");

        arguments.push(format!("{x}#{id}"));

        for dependency in dependencies {
            arguments.push(dependency.name().clone());
        }

        let arguments = arguments.into_boxed_slice();

        // dbg!(&arguments);

        let make_function_instruction = CompiledItem::Instruction {
            id: MAKE_FUNCTION,
            arguments,
        };

        Ok(vec![make_function_instruction, instruction!(store_fast id)])
    }
}

impl WalkForType for MemberFunction {
    fn type_from_node(input: &Node) -> Result<Ident> {
        let mut children = input.children();

        let ident_node = children.next().unwrap();
        let mut ident = Parser::ident(ident_node)?;

        let parameters_node = children.next().unwrap();
        let parameters = Parser::function_parameters(parameters_node, false, true)?;

        let maybe_return_type_node = children.next().unwrap();

        let return_type = if maybe_return_type_node.as_rule() == Rule::function_return_type {
            ScopeReturnStatus::Should(Parser::function_return_type(maybe_return_type_node)?)
        } else {
            ScopeReturnStatus::Void
        };

        let function_type = FunctionType::new(Arc::new(parameters), return_type);

        ident.link_force_no_inherit(
            input.user_data(),
            Cow::Owned(TypeLayout::Function(function_type)),
        )?;

        Ok(ident)
    }
}

impl MemberFunction {
    pub fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl Dependencies for MemberFunction {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        self.body.net_dependencies()
    }

    fn supplies(&self) -> Vec<crate::ast::Dependency> {
        self.parameters.supplies()
    }
}

impl Parser {
    pub fn class_bound_function(input: Node) -> Result<MemberFunction, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let mut ident = Self::ident(ident).to_err_vec()?;

        let parameters = children.next().unwrap();
        let maybe_body = children.next().unwrap();

        let (body, return_type) = if maybe_body.as_rule() == Rule::function_return_type {
            let return_type = Self::function_return_type(maybe_body).to_err_vec()?;

            let body = children.next().unwrap();

            (body, ScopeReturnStatus::Should(return_type))
        } else {
            (maybe_body, ScopeReturnStatus::Void)
        };

        // We need to keep the handle alive so that it is dropped at the end of this scope.
        // Using "_" or not saving it as a variable causes the scope to pop instantly.
        let _scope_handle = input.user_data().push_function(return_type.clone());

        let parameters = Self::function_parameters(parameters, true, true).to_err_vec()?;
        let body = Self::block(body)?;
        let function_type = FunctionType::new(Arc::new(parameters.clone()), return_type);

        ident.set_type_no_link(Cow::Owned(TypeLayout::Function(function_type)));

        let class_type = input.user_data().get_type_of_executing_class().unwrap();

        Ok(MemberFunction {
            ident,
            parameters,
            path_str: input.user_data().get_file_name(),
            body,
            class_id: class_type.id,
            class_name: class_type.arced_name(),
        })
    }
}
