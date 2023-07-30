use super::Primitive;
use crate::context::Ctx;
use crate::function::InstructionExitState;
use crate::instruction::JumpRequest;
use crate::rc_to_ref;
use crate::stack::{VariableFlags, VariableMapping};
use anyhow::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::rc::Rc;

pub struct MappedObject<'a> {
    name: &'a str,
    fields: Box<VariableMapping>,
}

pub struct ObjectBuilder {
    name: Option<Rc<String>>,
    object_variables: Option<Rc<VariableMapping>>,
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

    pub fn object_variables(&mut self, object_variables: Rc<VariableMapping>) -> &mut Self {
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
        let functions = self.functions.get(&name).expect("no functions");

        Object {
            name,
            object_variables: self.object_variables.clone().expect("no name"),
            functions,
        }
    }
}

#[doc(alias = "Class")]
#[derive(Debug)]
pub struct Object {
    pub name: Rc<String>,
    pub object_variables: Rc<VariableMapping>,
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
        object_variables: Rc<VariableMapping>,
    ) -> Self {
        Self {
            name,
            functions,
            object_variables,
        }
    }

    pub fn has_variable(
        &mut self,
        variable_name: &str,
    ) -> Option<Rc<(Primitive, VariableFlags)>> {
        rc_to_ref(&self.object_variables).get(variable_name)
    }

    pub fn has_function(&self, function_name: &String) -> bool {
        self.functions.contains(function_name)
    }

    pub fn get_assoc_function_name(&self, name: &String) -> Result<&String> {
        let x = self
            .functions
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
        let destination = self.get_assoc_function_name(function_name)?.clone();

        ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
            destination: crate::instruction::JumpRequestDestination::Standard(destination),
            callback_state: Some(Rc::clone(&self.object_variables)),
            stack: ctx.rced_call_stack(),
            arguments,
        }));

        Ok(())
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[object {} @ {:#x}] {{ {} }}",
            self.name, self as *const Self as usize, self.object_variables
        )
    }
}
