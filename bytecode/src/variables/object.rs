use crate::stack::{PrimitiveFlagsPair, VariableMapping};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;
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

        Object {
            name: Some(name),
            object_variables: self.object_variables.clone().expect("no name"),
            debug_lock: Rc::new(DebugPrintableLock::new()),
        }
    }
}

#[derive(Clone, Eq)]
struct DebugPrintableLock(Cell<bool>);

impl PartialEq for DebugPrintableLock {
    /// no-impl makes this field invisible
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Hash for DebugPrintableLock {
    /// no-impl makes this field invisible
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

impl DebugPrintableLock {
    pub fn new() -> Self {
        Self(Cell::new(true))
    }

    pub fn can_write(&self) -> bool {
        self.0.get()
    }

    pub fn lock(&self) {
        unsafe {
            let ptr = self.0.as_ptr();
            assert!(*ptr, "DebugPrintableAlreadyLockedError");
            *ptr = false;
        }
    }

    pub fn release(&self) {
        unsafe {
            let ptr = self.0.as_ptr();
            assert!(!*ptr, "DebugPrintableAlreadyReleasedError");
            *ptr = true;
        }
    }
}

pub struct Object {
    pub name: Option<Rc<String>>,
    pub object_variables: Rc<VariableMapping>,
    #[doc(hidden)]
    debug_lock: Rc<DebugPrintableLock>,
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.debug_lock.can_write() {
            self.debug_lock.lock();

            let result = f
                .debug_struct("Object")
                .field("name", &self.name)
                .field("object_variables", &self.object_variables)
                .finish();

            self.debug_lock.release();

            result
        } else if let Some(ref name) = self.name {
            write!(f, "<self-reference to {}>", name)
        } else {
            write!(f, "...")
        }
    }
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
    pub fn new(name: Rc<String>, object_variables: Rc<VariableMapping>) -> Self {
        Self {
            name: Some(name),
            object_variables,
            debug_lock: Rc::new(DebugPrintableLock::new()),
        }
    }

    pub fn get_property(
        &self,
        property_name: &str,
        include_functions: bool,
    ) -> Option<PrimitiveFlagsPair> {
        let maybe_property = self.has_variable(property_name);

        if maybe_property.is_some() {
            return maybe_property;
        }

        let include_class_name = if let (true, Some(name)) =
            (include_functions, self.name.as_ref().map(|x| x.as_str()))
        {
            Some(name)
        } else {
            None
        };

        self.has_function(property_name, include_class_name)
    }

    pub fn has_variable(&self, variable_name: &str) -> Option<PrimitiveFlagsPair> {
        self.object_variables.get(variable_name)
    }

    pub fn has_function(
        &self,
        function_name: &str,
        include_class_name: Option<&str>,
    ) -> Option<PrimitiveFlagsPair> {
        if let Some(class_name) = include_class_name {
            let joined_name = class_name.to_owned() + "::" + function_name;
            let maybe_function_ptr = self.object_variables.get(&joined_name);
            if let Some(bundle) = maybe_function_ptr {
                let bundle_view = bundle.primitive().is_function();

                if bundle_view {
                    return Some(bundle);
                }
            }
        }

        let direct_lookup = self.object_variables.get(function_name);

        let Some(field) = direct_lookup else {
            return None;
        };

        let bundle = field.primitive().is_function();

        if bundle {
            Some(field)
        } else {
            None
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref name) = self.name {
            write!(f, "<class {} @ {:#x}>", name, self as *const Self as usize,)?;
        } else {
            write!(f, "object@{:#x}", self as *const Self as usize)?;
        }

        if cfg!(feature = "debug") {
            write!(f, " (--debug {:?})", self.object_variables)?;
        }

        Ok(())
    }
}
