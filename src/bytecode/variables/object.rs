use super::{Primitive, Variable};
use crate::bytecode::context::Ctx;
use crate::bytecode::function::InstructionExitState;
use crate::bytecode::instruction::JumpRequest;
use crate::bytecode::stack::VariableMapping;
use anyhow::{Result, Context};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
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

    pub fn build(&'static mut self) -> Object {
        let name = self.name.clone().unwrap();
        let functions =
            self.functions.get(&name).expect("no functions");

        let result = Object {
            name,
            object_variables: self.object_variables.clone().expect("no name"),
            functions,
        };

        result
    }
}

#[derive(Debug)]
pub struct Object {
    pub name: Rc<String>,
    pub object_variables: Arc<VariableMapping>,
    pub functions: &'static HashSet<String>,
}

impl PartialOrd for Object {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        unimplemented!("this must be done with a function call, not natively.")
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
        functions: &'static HashSet<String>,
        object_variables: Arc<VariableMapping>,
    ) -> Self {
        Self {
            name,
            functions,
            object_variables,
        }
    }

	pub fn has_variable_mut(&mut self, variable_name: &String) -> Option<&mut Variable> {
		let object_variables = Arc::as_ptr(&self.object_variables) as *mut VariableMapping;

		unsafe {
			(*object_variables).0.get_mut(variable_name)
		}
	}

    pub fn has_function(&self, function_name: &String) -> bool {
		self.functions.contains(function_name)
    }

    pub fn get_assoc_function_name(&self, name: &String) -> Result<&String> {
        let x = self.functions
            .get(name)
            .with_context(|| format!("Object {} does not have method {name}", self.name))?;
        Ok(x)
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

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[object {}]", self.name)
    }
}
