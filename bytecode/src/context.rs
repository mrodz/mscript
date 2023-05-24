use super::arc_to_ref;
use super::function::{Function, InstructionExitState};
use super::stack::VariableMapping;
use super::variables::Primitive;
use crate::stack::{Stack, VariableFlags};
use anyhow::{bail, Result};
use std::borrow::Cow;
use std::fmt::{Debug, Display};
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
pub enum SpecialScope {
    If,
    Else,
}

impl Display for SpecialScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::If => "<if>",
                Self::Else => "<else>",
            }
        )
    }
}

#[derive(Debug)]
pub struct Ctx<'a> {
    stack: Vec<Primitive>,
    function: &'a Function,
    call_stack: Arc<Stack>,
    exit_state: InstructionExitState,
    args: Cow<'a, Vec<Primitive>>,
    callback_state: Option<Arc<VariableMapping>>,
    special_scopes: Option<&'a mut Vec<SpecialScope>>,
}

impl<'a> Ctx<'a> {
    pub fn new(
        function: &'a Function,
        call_stack: Arc<Stack>,
        args: Cow<'a, Vec<Primitive>>,
        callback_state: Option<Arc<VariableMapping>>,
    ) -> Self {
        Self {
            stack: Vec::new(),
            function,
            call_stack,
            exit_state: InstructionExitState::NoExit,
            args,
            callback_state,
            special_scopes: None,
        }
    }

    pub fn update_callback_variable(&mut self, name: String, value: Primitive) -> Result<()> {
        let Some(ref mapping) = self.callback_state else {
            bail!("this function is not a callback")
        };

        let mapping = Arc::as_ptr(mapping) as *mut VariableMapping;

        unsafe {
            (*mapping).update(name, value)?;
        }

        Ok(())
    }

    pub fn load_callback_variable(&self, name: &String) -> Result<&Primitive> {
        let Some(ref mapping) = self.callback_state else {
            bail!("this function is not a callback")
        };

        let Some(pair) = mapping.get(name) else {
            bail!("this callback does not have `{name}`")
        };

        let primitive = &pair.0;

        Ok(primitive)
    }

    pub fn owner(&self) -> &Function {
        self.function
    }

    pub fn arced_call_stack(&self) -> Arc<Stack> {
        Arc::clone(&self.call_stack)
    }

    pub fn argc(&self) -> usize {
        self.args.len()
    }

    pub fn nth_arg(&self, n: usize) -> Option<&Primitive> {
        self.args.get(n)
    }

    pub fn get_nth_op_item(&self, n: usize) -> Option<&Primitive> {
        self.stack.get(n)
    }

    pub fn get_nth_op_item_mut(&mut self, n: usize) -> Option<&mut Primitive> {
        self.stack.get_mut(n)
    }

    pub fn get_last_op_item(&mut self) -> Option<&Primitive> {
        self.stack.get(self.stack_size() - 1)
    }

    pub fn get_last_op_item_mut(&mut self) -> Option<&mut Primitive> {
        let last_idx = self.stack_size() - 1;
        self.stack.get_mut(last_idx)
    }

    pub fn get_call_stack_string(&self) -> String {
        self.call_stack.to_string()
    }

    pub fn get_local_operating_stack(&self) -> Vec<Primitive> {
        self.stack.clone()
    }

    pub(crate) fn signal(&mut self, exit_state: InstructionExitState) {
        self.exit_state = exit_state;
    }

    pub(crate) fn poll(&self) -> &InstructionExitState {
        &self.exit_state
    }

    pub(crate) fn clear_signal(&mut self) {
        self.exit_state = InstructionExitState::NoExit;
    }

    pub(crate) fn add_frame(&self, label: String) {
        arc_to_ref(&self.call_stack).extend(label)
    }

    pub(crate) fn pop_frame(&self) {
        arc_to_ref(&self.call_stack).pop()
    }

    pub(crate) fn frames_count(&self) -> usize {
        unsafe { (*Arc::as_ptr(&self.call_stack)).size() }
    }

    pub(crate) fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub(crate) fn clear_and_set_stack(&mut self, var: Primitive) {
        self.stack.clear();
        self.stack.push(var);
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }

    pub(crate) fn push(&mut self, var: Primitive) {
        self.stack.push(var);
    }

    pub(crate) fn pop(&mut self) -> Option<Primitive> {
        self.stack.pop()
    }

    pub(crate) fn register_variable(&self, name: String, var: Primitive) {
        arc_to_ref(&self.call_stack).register_variable(name, var)
    }

    pub(crate) fn update_variable(&self, name: String, var: Primitive) -> Result<()> {
        arc_to_ref(&self.call_stack).update_variable(name, var)
    }

    pub(crate) fn load_variable(&self, name: &String) -> Option<&(Primitive, VariableFlags)> {
        self.call_stack.find_name(name)
    }

    pub(crate) fn get_frame_variables(&self) -> &VariableMapping {
        self.call_stack.get_frame_variables()
    }

    pub(crate) fn load_local(&self, name: &String) -> Option<&(Primitive, VariableFlags)> {
        self.call_stack.get_frame_variables().get(name)
    }
}
