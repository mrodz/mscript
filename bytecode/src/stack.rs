use anyhow::{Context, Result};
use std::collections::HashMap;
use std::fmt::{Debug, Display};

use super::variables::Primitive;

#[derive(Default, Debug, PartialEq, Clone)]
pub struct VariableMapping(pub HashMap<String, Primitive>);

impl Display for VariableMapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = vec![];

        for (key, value) in self.0.iter() {
            result.push(format!("{key} = {value}"));
        }

        let string = if result.len() == 0 {
            "None".into()
        } else {
            let mut ret = String::new();

            ret.push_str(&result[0]);

            for combo in &result[1..] {
                ret.push_str(", ");
                ret.push_str(&combo);
            }

            ret
            // result.iter().fold("".to_owned(), |x, y| x +  + y)
        };

        write!(f, "{string}")
    }
}

impl VariableMapping {
    pub fn get(&self, key: &String) -> Option<&Primitive> {
        self.0.get(key)
    }

    pub fn get_mut(&mut self, key: &String) -> Option<&mut Primitive> {
        self.0.get_mut(key)
    }

    pub fn update(&mut self, key: String, value: Primitive) -> Result<()> {
        // if insert returns None, that means there was no value there,
        // and this `update` is invalid.
        let _ = self
            .0
            .insert(key, value)
            .context("variable has not been mapped")?;
        Ok(())
    }
}

#[derive(Debug)]
struct StackFrame {
    label: String,
    variables: VariableMapping,
}

#[derive(Debug)]
pub struct Stack(Vec<StackFrame>);

impl Stack {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn get_frame_label(&self) -> &String {
        &self.0.last().expect("nothing in the stack").label
    }

    pub fn get_frame_variables(&self) -> &VariableMapping {
        &self.0.last().expect("nothing in the stack").variables
    }

    pub fn extend(&mut self, label: String) {
        self.0.push(StackFrame {
            label: label,
            variables: VariableMapping::default(),
        });
    }

    pub fn size(&self) -> usize {
        self.0.len()
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn find_name(&self, name: &String) -> Option<&Primitive> {
        for i in (0..=self.size() - 1).rev() {
            if let Some(var) = self.0[i].variables.0.get(name) {
                return Some(var);
            }
        }

        None
    }

    pub fn register_variable(&mut self, name: String, var: Primitive) {
        self.0
            .last_mut()
            .expect("nothing in the stack")
            .variables
            .0
            .insert(name, var);
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
