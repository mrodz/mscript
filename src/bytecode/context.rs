use std::{cell::Cell, fmt::Debug, sync::Arc, collections::VecDeque};

use anyhow::{bail, Result};

use super::{
    attributes_parser::Attributes,
    file::IfStatement,
    function::{Function, InstructionExitState},
    stack::VariableMapping,
    variables::{Primitive, Variable},
    Stack,
};

pub struct Ctx<'a> {
    stack: VecDeque<Primitive>,
    function: &'a Function,
    call_stack: Arc<Cell<Stack>>,
    pub active_if_stmts: Vec<(IfStatement, bool)>,
    exit_state: InstructionExitState,
    args: Vec<Primitive>,
    callback_state: Option<Arc<VariableMapping>>,
}

impl Debug for Ctx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ctx")
    }
}

impl<'a> Ctx<'a> {
    pub fn new(
        function: &'a Function,
        call_stack: Arc<Cell<Stack>>,
        args: Vec<Primitive>,
        callback_state: Option<Arc<VariableMapping>>,
    ) -> Self {
        Self {
            stack: VecDeque::new(),
            active_if_stmts: vec![],
            function,
            call_stack,
            exit_state: InstructionExitState::NoExit,
            args,
            callback_state,
        }
    }

    pub fn load_callback_variable(&self, name: &String) -> Result<Option<&Variable>> {
        let Some(ref mapping) = self.callback_state else {
            bail!("this function is not a callback")
        };

        Ok(mapping.get(name))
    }

    pub fn owner(&self) -> &Function {
        self.function
    }

    pub fn arced_call_stack(&self) -> Arc<Cell<Stack>> {
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

    pub fn get_call_stack(&self) -> &mut Stack {
        use std::borrow::BorrowMut;
        unsafe { (*self.call_stack.as_ptr()).borrow_mut() }
    }

    pub fn get_local_operating_stack(&self) -> VecDeque<Primitive> {
        self.stack.clone()
    }

    pub fn get_attributes(&self) -> &Vec<Attributes> {
        &self.function.attributes
    }

    pub(crate) fn signal(&mut self, exit_state: InstructionExitState) {
        self.exit_state = exit_state;
    }

    pub(crate) fn poll(&mut self) -> InstructionExitState {
        self.exit_state.clone()
    }

    pub(crate) fn clear_signal(&mut self) {
        self.exit_state = InstructionExitState::NoExit;
    }

    pub(crate) fn add_frame(&self, label: String) {
        unsafe { (*self.call_stack.as_ptr()).extend(label) }
    }

    pub(crate) fn frames_count(&self) -> usize {
        unsafe { (*self.call_stack.as_ptr()).size() }
    }

    pub(crate) fn clear_stack(&mut self) {
        self.stack.clear();
    }

    pub(crate) fn clear_and_set_stack(&mut self, var: Primitive) {
        self.stack.clear();
        self.stack.push_back(var);
    }

    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }

    pub(crate) fn push(&mut self, var: Primitive) {
        self.stack.push_back(var);
    }

    pub(crate) fn pop_front(&mut self) -> Option<Primitive> {
        self.stack.pop_front()
    }

    pub(crate) fn pop(&mut self) -> Option<Primitive> {
        self.stack.pop_back()
    }

    pub(crate) fn register_variable(&self, name: String, var: Primitive) {
        unsafe { (*self.call_stack.as_ptr()).register_variable(name, var) }
    }

    pub(crate) fn load_variable(&self, name: &String) -> Option<&Variable> {
        unsafe { (*self.call_stack.as_ptr()).find_name(name) }
    }

    pub(crate) fn load_local(&self, name: &String) -> Option<&Variable> {
        unsafe { (*self.call_stack.as_ptr()).get_frame_variables().get(name) }
    }
}
