use super::Primitive;
use crate::bytecode::context::Ctx;
use crate::bytecode::function::InstructionExitState;
use crate::bytecode::instruction::JumpRequest;
use crate::bytecode::stack::VariableMapping;
use anyhow::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::rc::Rc;
use std::sync::Arc;

pub struct ObjectBuilder {
    name: Option<Rc<String>>,
    object_variables: Option<Arc<VariableMapping>>,
    functions: HashMap<Rc<String>, HashSet<String>>,
}

impl ObjectBuilder {
    pub fn new() -> Self {
        Self {
            name: None,
            object_variables: None,
            functions: HashMap::new(),
        }
    }

    pub fn name(&mut self, name: Rc<String>) -> &mut Self {
        self.name = Some(name);
        self
    }

    pub fn object_variables(&mut self, object_variables: Arc<VariableMapping>) -> &mut Self {
        self.object_variables = Some(object_variables);
        self
    }

    pub fn has_class_been_registered(&self, name: &String) -> bool {
        self.functions.contains_key(name)
    }

    pub fn register_class(&mut self, class_name: Rc<String>, functions: HashSet<String>) {
        self.functions.insert(class_name, functions);
    }

    pub fn build(&mut self) -> Object {
        let name = self.name.clone().unwrap();
        let functions: *mut HashSet<String> =
            self.functions.get_mut(&name).expect("no functions");

        let result = Object {
            name,
            object_variables: self.object_variables.clone().expect("no name"),
            functions,
        };

		*self = ObjectBuilder::new();

        result
    }
}

#[derive(Clone)]
pub struct Object {
    pub name: Rc<String>,
    pub object_variables: Arc<VariableMapping>,
    pub functions: *mut HashSet<String>,
}

impl PartialOrd for Object {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        unimplemented!()
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.object_variables == other.object_variables
    }
}

impl Object {
    pub fn new(
        name: Rc<String>,
        functions: *mut HashSet<String>,
        object_variables: Arc<VariableMapping>,
    ) -> Self {
        Self {
            name,
            functions,
            object_variables,
        }
    }
    pub fn has_function(&self, function_name: &String) -> bool {
        unsafe { (*self.functions).contains(function_name) }
    }

    pub fn get_assoc_function_name(&self, name: &String) -> Result<&String> {
        unsafe {
            let x = (*self.functions)
                .get(name)
                .with_context(|| format!("Object {} does not have method {name}", self.name))?;
            Ok(x)
        }
    }

    pub fn call_fn(
        &self,
        function_name: &String,
        arguments: Vec<Primitive>,
        ctx: &mut Ctx,
    ) -> Result<()> {
        let destination_label = self.get_assoc_function_name(function_name)?.clone();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination_label,
            callback_state: Some(Arc::clone(&self.object_variables)),
            stack: ctx.arced_call_stack().clone(),
            arguments,
        }));

        Ok(())
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[object {}]", self.name)
    }
}
