//! Program call stack

use anyhow::{bail, Result};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::rc_to_ref;

use super::variables::Primitive;

/// Bit fields for variable flags.
pub(crate) mod flag_constants {
    pub const READ_ONLY: u8 = 0b00000001;
    pub const PUBLIC: u8 = 0b00000010;
    pub const LOCAL_FRAME_ONLY: u8 = 0b00000100;
    pub const LOOP_VARIABLE: u8 = 0b00001000;    
}

#[derive(PartialEq, Clone)]
pub struct VariableFlags(u8);

impl VariableFlags {
    #[inline(always)]
    pub fn none() -> Self {
        VariableFlags(0)
    }

    #[inline(always)]
    pub fn is_read_only(&self) -> bool {
        self.0 & flag_constants::READ_ONLY == flag_constants::READ_ONLY
    }

    #[inline(always)]
    pub fn can_update(&self) -> bool {
        !self.is_read_only()
    }

    #[inline(always)]
    pub fn is_public(&self) -> bool {
        self.0 & flag_constants::PUBLIC == flag_constants::PUBLIC
    }

    #[inline(always)]
    pub fn is_exclusive_to_frame(&self) -> bool {
        self.0 & flag_constants::LOCAL_FRAME_ONLY == flag_constants::LOCAL_FRAME_ONLY
    }

    pub fn is_loop_variable(&self) -> bool {
        self.0 & flag_constants::LOOP_VARIABLE == flag_constants::LOOP_VARIABLE
    }
}

impl Debug for VariableFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buffer = vec![];

        if self.0 & flag_constants::READ_ONLY == flag_constants::READ_ONLY {
            buffer.push("READ_ONLY")
        }

        if self.0 & flag_constants::PUBLIC == flag_constants::PUBLIC {
            buffer.push("PUBLIC")
        } else {
            buffer.push("PRIVATE")
        }

        write!(f, "{buffer:?}")
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct VariableMapping(HashMap<String, Rc<(Primitive, VariableFlags)>>);

impl Display for VariableMapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = vec![];

        for (key, value) in self.0.iter() {
            result.push(format!("{key} = {} (attr: {:?})", value.0, value.1));
        }

        let string = if result.is_empty() {
            "None".into()
        } else {
            let mut ret = String::new();

            ret.push_str(&result[0]);

            for combo in &result[1..] {
                ret.push_str(", ");
                ret.push_str(combo);
            }

            ret
        };

        write!(f, "{string}")
    }
}

impl From<HashMap<String, Rc<(Primitive, VariableFlags)>>> for VariableMapping {
    fn from(value: HashMap<String, Rc<(Primitive, VariableFlags)>>) -> Self {
        Self(value)
    }
}

impl VariableMapping {
    pub fn get(&self, key: &String) -> Option<Rc<(Primitive, VariableFlags)>> {
        self.0.get(key).cloned()
    }

    /// Update the value of a variable that has already been registered. 
    /// The variable will keep **ALL** of the flags from its previous mapping.
    /// 
    /// # Errors
    /// This function can error if the variable has not been mapped, or if the name is read-only.
    pub fn update(&mut self, name: &String, value: Primitive) -> Result<()> {
        // if insert returns None, that means there was no value there,
        // and this `update` is invalid.
        if let Some(pair) = self.0.get_mut(name) {
            if pair.1.can_update() {
                rc_to_ref(pair).0 = value;
            } else {
                bail!("variable is read-only")
            }
        } else {
            bail!("variable has not been mapped")
        }

        Ok(())
    }
}

/// A stack frame in the MScript interpreter. Each stack frame has a name and
/// variables associated with the frame.
#[derive(Debug)]
struct StackFrame {
    label: String,
    variables: VariableMapping,
}

/// The call stack of an interpreter. Standard LIFO implementation.
#[derive(Debug, Default)]
pub struct Stack(Vec<StackFrame>);

impl Stack {
    /// Create a new, empty call stack.
    pub fn new() -> Self {
        Self(vec![])
    }

    /// Get the label of the current frame.
    pub fn get_frame_label(&self) -> &String {
        &self.0.last().expect("nothing in the stack").label
    }

    /// Get the variables of the current frame.
    pub fn get_frame_variables(&self) -> &VariableMapping {
        &self.0.last().expect("nothing in the stack").variables
    }

    /// Extend the call stack by adding a new frame.
    pub fn extend(&mut self, label: String) {
        self.0.push(StackFrame {
            label,
            variables: VariableMapping::default(),
        });
    }

    /// Get the number of stack frames in use.
    pub fn size(&self) -> usize {
        self.0.len()
    }

    /// Pop the top of the stack frame, releasing all of its resources.
    pub fn pop(&mut self) {
        self.0.pop();
    }

    /// Search the call stack for a frame with a variable with a matching `name`. Will start at the top (most recent)
    /// and will continue until the very first stack frame.
    pub fn find_name(&self, name: &String) -> Option<Rc<(Primitive, VariableFlags)>> {
        for stack_frame in self.0.iter().rev() {
            let tuple = stack_frame.variables.get(name);
            if let Some(ref packed) = tuple {
                let flags = &packed.1;

                if !flags.is_exclusive_to_frame() {
                    return Some(tuple.unwrap());
                } else {
                    continue;
                }
            }
        }

        None
    }

    /// Add a `name -> variable` mapping to the current stack frame, with default flags.
    pub fn register_variable(&mut self, name: String, var: Primitive) {
        self.register_variable_flags(name, var, VariableFlags::none());
    }

    /// Add a `name -> variable` mapping to the current stack frame, with special flags.
    pub fn register_variable_flags(&mut self, name: String, var: Primitive, flags: VariableFlags) {
        self.0
            .last_mut()
            .expect("nothing in the stack")
            .variables
            .0
            .insert(name, Rc::new((var, flags)));
    }

    /// Update the value of a variable, given its name. This function will search the entire
    /// call stack.
    pub fn update_variable(&mut self, name: String, new_var: Primitive) -> Result<()> {
        for stack_frame in self.0.iter_mut().rev() {
            if let Some(old_var) = stack_frame.variables.0.get_mut(&name) {
                if old_var.1.can_update() {
                    rc_to_ref(old_var).0 = new_var;
                }
                return Ok(());
            }
        }

        bail!("could not find variable {name} in the call stack.")
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(first) = self.0.last() else {
            return write!(f, "<Empty Stack>")
        };

        write!(f, "\t>> {}", first.label)?; // print the cause first

        for stack_frame in self.0[..self.size() - 1].iter().rev() {
            write!(f, "\r\n\t ^ {}", stack_frame.label)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::Stack;

    #[test]
    fn add_frame() {
        let mut stack = Stack::new();

        let one = String::from("Main");
        let two = String::from("hi");

        stack.extend(one);
        stack.extend(two);
    }
}
