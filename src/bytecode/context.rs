use std::{sync::Arc, cell::Cell, fmt::Debug};

use super::{variables::{Primitive, Variable}, function::{Function, InstructionExitState}, Stack, file::IfStatement, attributes_parser::Attributes};

pub struct Ctx<'a> {
    stack: Vec<Primitive>,
    function: &'a Function,
    call_stack: Arc<Cell<Stack>>,
    pub active_if_stmts: Vec<(IfStatement, bool)>,
    exit_state: InstructionExitState,
	args: Vec<Primitive>,
}

impl Debug for Ctx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ctx")
    }
}

impl<'a> Ctx<'a> {
    pub fn new(function: &'a Function, call_stack: Arc<Cell<Stack>>, args: Vec<Primitive>) -> Self {
        Self {
            stack: vec![],
            active_if_stmts: vec![],
            function,
            call_stack,
            exit_state: InstructionExitState::NoExit,
			args,
        }
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

	pub fn get_call_stack(&self) -> &mut Stack {
		use std::borrow::BorrowMut;
		unsafe {
			(*self.call_stack.as_ptr()).borrow_mut()
		}
	}

	pub fn get_local_operating_stack(&self) -> Vec<Primitive> {
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
        unsafe { (*self.call_stack.as_ptr()).register_variable(name, var) }
    }

    pub(crate) fn load_variable(&self, name: &String) -> Option<&Variable> {
        unsafe { (*self.call_stack.as_ptr()).find_name(name) }
    }

    pub(crate) fn load_local(&self, name: &String) -> Option<&Variable> {
        unsafe { (*self.call_stack.as_ptr()).get_frame_variables().get(name) }
    }
}
