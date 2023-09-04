use super::Primitive;
use crate::function::PrimitiveFunction;
use crate::rc_to_ref;
use crate::stack::{flag_constants, VariableFlags, VariableMapping};
use std::borrow::Cow;
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
        // let functions = self.functions.get(&name).expect("no functions");

        Object {
            name,
            object_variables: self.object_variables.clone().expect("no name"),
            // functions,
        }
    }
}

#[doc(alias = "Class")]
#[derive(Debug)]
pub struct Object {
    pub name: Rc<String>,
    pub object_variables: Rc<VariableMapping>,
    // pub functions: &'static HashSet<String>,
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
        // functions: &'static HashSet<String>,
        object_variables: Rc<VariableMapping>,
    ) -> Self {
        Self {
            name,
            // functions,
            object_variables,
        }
    }

    pub fn get_property(
        &mut self,
        property_name: &str,
        include_functions: bool,
    ) -> Option<Rc<(Primitive, VariableFlags)>> {
        let maybe_property = self.has_variable(property_name);

        if maybe_property.is_some() {
            return maybe_property;
        }

        let include_class_name = if include_functions {
            Some(self.name.as_str())
        } else {
            None
        };

        let is_function = self.has_function(property_name, include_class_name);

        if let Some(path_to_use) = is_function {
            let function = PrimitiveFunction::new(path_to_use.into_owned(), None);
            Some(Rc::new((
                Primitive::Function(function),
                VariableFlags(flag_constants::READ_ONLY),
            )))
        } else {
            None
        }
    }

    pub fn has_variable(&self, variable_name: &str) -> Option<Rc<(Primitive, VariableFlags)>> {
        rc_to_ref(&self.object_variables).get(variable_name)
    }

    pub fn has_function<'a>(
        &self,
        function_name: &'a str,
        include_class_name: Option<&str>,
    ) -> Option<Cow<'a, str>> {
        if let Some(class_name) = include_class_name {
            let joined_name = class_name.to_owned() + "::" + function_name;
            let maybe_function_ptr = self.object_variables.get(&joined_name);
            if let Some(bundle) = maybe_function_ptr {
                if let Primitive::Function(function_ptr) = &bundle.0 {
                    return Some(Cow::Owned(function_ptr.location().clone()));
                }
            }
        }

        if self.object_variables.contains(function_name) {
            Some(Cow::Borrowed(function_name))
        } else {
            None
        }
    }

    // pub fn get_assoc_function_name(&self, name: &String) -> Result<&String> {
    //     let x = self
    //         .functions
    //         .get(name)
    //         .with_context(|| format!("Object {} does not have method {name}", self.name))?;
    //     Ok(x)
    // }

    // pub fn call_fn(
    //     &self,
    //     function_name: &String,
    //     arguments: Vec<Primitive>,
    //     ctx: &mut Ctx,
    // ) -> Result<()> {
    //     let destination = self.get_assoc_function_name(function_name)?.clone();

    //     ctx.signal(InstructionExitState::JumpRequest(JumpRequest {
    //         destination: crate::instruction::JumpRequestDestination::Standard(destination),
    //         callback_state: Some(Rc::clone(&self.object_variables)),
    //         stack: ctx.rced_call_stack(),
    //         arguments,
    //     }));

    //     Ok(())
    // }
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
