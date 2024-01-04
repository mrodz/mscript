//! This is the API through which all instructions interact with each other and the program at large.
//! In other words, all side effects should carried out through this interface.

use super::function::{Function, InstructionExitState};
use super::stack::VariableMapping;
use super::variables::Primitive;
use crate::stack::{PrimitiveFlagsPair, Stack, VariableFlags};
use anyhow::{bail, Context, Result};
use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::slice::SliceIndex;

/// Used to differentiate between different scope types, mostly for debug purposes.
#[doc(alias = "Scope")]
#[derive(Debug, Clone, Copy)]
pub enum SpecialScope {
    /// The scope of an `If` statement
    If,
    /// The scope of an `Else` statement
    Else,
    WhileLoop,
}

static DISPLAY_NAME_IF_SCOPE: &str = "<if>";
static DISPLAY_NAME_ELSE_SCOPE: &str = "<else>";
static DISPLAY_NAME_WHILE_LOOP_SCOPE: &str = "<while>";

impl SpecialScope {
    pub fn is_label_special_scope(label: &str) -> bool {
        label == DISPLAY_NAME_IF_SCOPE
            || label == DISPLAY_NAME_ELSE_SCOPE
            || label == DISPLAY_NAME_WHILE_LOOP_SCOPE
    }
}

impl Display for SpecialScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::If => DISPLAY_NAME_IF_SCOPE,
                Self::Else => DISPLAY_NAME_ELSE_SCOPE,
                Self::WhileLoop => DISPLAY_NAME_WHILE_LOOP_SCOPE,
            }
        )
    }
}

/// Stores information about the current function call.
///
/// This struct exposes the API through which bytecode instructions
/// can interact both with each other and the program at large.
#[derive(Debug)]
pub struct Ctx<'a> {
    /// The Local Operating Stack that serves as the backbone for the interpreter.
    /// Each function in the call stack has its own local operating stack,
    /// which can be used as a [Stack Machine](https://en.wikipedia.org/wiki/Stack_machine)
    /// to compute and evaluate.
    stack: Vec<Primitive>,
    /// A reference to the function that this [`Ctx`] belongs to.
    function: &'a Function,
    /// A shared reference to the interpreter's function stack trace.
    call_stack: Rc<RefCell<Stack>>,
    /// This field is checked after executing each instruction. While private, it can
    /// be modified via the [`Ctx::signal`] and [`Ctx::clear_signal`] methods.
    exit_state: Box<InstructionExitState>,
    /// The arguments to the function.
    args: Cow<'a, Vec<Primitive>>,
    /// A separate variable mapping for closures and objects. If `None`, this function is neither.
    /// Otherwise, contains a reference shared amongst all instances of the callback to point to
    /// shared/global data.
    callback_state: Option<Rc<VariableMapping>>,
}

impl<'a> Ctx<'a> {
    /// Initializes the function context.
    ///
    /// # Arguments
    ///
    /// * `function` - A reference to the [`Function`] it belongs to
    /// * `call_stack` - A shared reference to the program call stack (used for jumping)
    /// * `args` - The arguments to the function
    /// * `callback_state` - Any special variables captured by the function
    pub fn new(
        function: &'a Function,
        call_stack: Rc<RefCell<Stack>>,
        args: Cow<'a, Vec<Primitive>>,
        callback_state: Option<Rc<VariableMapping>>,
    ) -> Self {
        Self {
            stack: Vec::new(),
            function,
            call_stack,
            exit_state: Box::new(InstructionExitState::NoExit),
            args,
            callback_state,
            // special_scopes: None,
        }
    }

    /// Mutates the shared [`Primitive`] stored as a callback variable.
    ///
    /// Will error if called on a [`Ctx`] that does not belong to a callback.
    /// Errors from the update operation will be propogated upwards through this function.
    ///
    /// # Arguments
    ///
    /// * `name` - the name of the callback variable
    /// * `value` - the [`Primitive`] value that will be stored in the same slot.
    pub fn update_callback_variable(&mut self, name: &str, value: Primitive) -> Result<()> {
        let Some(ref mapping) = self.callback_state else {
            bail!("this function is not a callback")
        };

        mapping.update(name, value)?;

        Ok(())
    }

    pub fn get_file_module(&self) -> Primitive {
        let location = self
            .function
            .location()
            .upgrade()
            .expect("could not get a reference to the backing file");
        Primitive::Module(location.get_exports())
    }

    /// Loads the shared [`Primitive`] stored as a callback variable, along with its associated [`VariableFlags`].
    ///
    /// Will error if called on a [`Ctx`] that does not belong to a callback.
    /// Errors from the update operation will be propogated upwards through this function.
    ///
    /// # Arguments
    ///
    /// * `name` - the name of the callback variable
    /// * `value` - the [`Primitive`] value that will be stored in the same slot.
    pub fn load_callback_variable(&self, name: &str) -> Result<PrimitiveFlagsPair> {
        let Some(ref mapping) = self.callback_state else {
            bail!("this function is not a callback")
        };

        let Some(pair) = mapping.get(name) else {
            bail!("this callback does not have `{name}`")
        };

        Ok(pair)
    }

    /// Returns the [`Function`] associated with this [`Ctx`]. In a sense, the bytecode function "owns" this context, hence the name.
    pub fn owner(&self) -> &Function {
        self.function
    }

    /// Returns the [Call Stack](`Stack`) associated with this [`Ctx`] as an `Rc`.
    pub fn rced_call_stack(&self) -> Rc<RefCell<Stack>> {
        Rc::clone(&self.call_stack)
    }

    /// Returns the length of the arguments to the function that owns this [`Ctx`]
    pub fn argc(&self) -> usize {
        self.args.len()
    }

    /// Returns the Nth argument of the function that owns this [`Ctx`]
    pub fn nth_arg(&self, n: usize) -> Option<&Primitive> {
        self.args.get(n)
    }

    /// Returns a reference to the Nth item on the local operating stack of the function that owns this [`Ctx`]
    pub fn get_nth_op_item(&self, n: usize) -> Option<&Primitive> {
        self.stack.get(n)
    }

    /// Returns a mutable reference to the Nth item on the local operating stack of the function that owns this [`Ctx`]
    pub fn get_nth_op_item_mut(&mut self, n: usize) -> Option<&mut Primitive> {
        self.stack.get_mut(n)
    }

    /// Return many mutable references to items on the local operating stack.
    pub fn get_many_op_items_mut<I>(&mut self, range: I) -> Option<&mut I::Output>
    where
        I: SliceIndex<[Primitive]>,
    {
        self.stack.get_mut(range)
    }

    /// Returns a reference to the last item on the local operating stack of the function that owns this [`Ctx`]
    pub fn get_last_op_item(&mut self) -> Option<&Primitive> {
        self.stack.get(self.stack_size() - 1)
    }

    /// Returns a mutable reference to the last item on the local operating stack of the function that owns this [`Ctx`]
    pub fn get_last_op_item_mut(&mut self) -> Option<&mut Primitive> {
        let last_idx = self.stack_size() - 1;
        self.stack.get_mut(last_idx)
    }

    /// Get the `Display` format of the call stack. Principally used for Debug/Display purposes.
    pub fn get_call_stack_string(&self) -> String {
        self.call_stack.borrow().to_string()
    }

    /// Creates a **COPY** of the local operating stack.
    pub fn get_local_operating_stack(&self) -> &Vec<Primitive> {
        &self.stack
    }

    pub fn ref_local_operating_stack(&self) -> &[Primitive] {
        &self.stack
    }

    /// Exit the instruction with an [`InstructionExitState`]. Each state
    /// is cleared before the next instruction is processed.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Ask the interpreter to jump forward +5 instructions once this instruction finishes.
    /// ctx.signal(InstructionExitState::Goto(5))
    /// ```
    pub(crate) fn signal(&mut self, exit_state: InstructionExitState) {
        *self.exit_state = exit_state;
    }

    /// Get the [`InstructionExitState`] stored in the [`Ctx`]
    pub(crate) fn poll(&self) -> &InstructionExitState {
        &self.exit_state
    }

    /// Reset [`Ctx::exit_state`]
    pub(crate) fn clear_signal(&mut self) {
        *self.exit_state = InstructionExitState::NoExit;
    }

    /// Add a new stack frame with a given label.
    pub(crate) fn add_frame(&self, label: String) {
        self.call_stack.borrow_mut().extend(label)
    }

    /// Pop a frame from the stack.
    pub(crate) fn pop_frame(&self) {
        self.call_stack.borrow_mut().pop()
    }

    /// Get the depth of the call stack.
    pub(crate) fn frames_count(&self) -> usize {
        self.call_stack.borrow().size()
    }

    /// Delete all items in the local operating stack.
    pub(crate) fn clear_stack(&mut self) {
        self.stack.clear();
    }

    /// Delete all items in the local operating stack, and insert `var`.
    pub(crate) fn clear_and_set_stack(&mut self, var: Primitive) {
        self.stack.clear();
        self.stack.push(var);
    }

    /// Get how many items are in the local operating stack.
    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }

    /// Push an item onto the top of the local operating stack.
    pub(crate) fn push(&mut self, var: Primitive) {
        self.stack.push(var);
    }

    pub(crate) fn push_front(&mut self, var: Primitive) {
        self.stack.insert(0, var);
    }

    /// Pop an item from the local operating stack.
    pub(crate) fn pop(&mut self) -> Option<Primitive> {
        self.stack.pop()
    }

    pub(crate) fn register_export(&self, name: String, var: PrimitiveFlagsPair) -> Result<()> {
        let file = self
            .function
            .location()
            .upgrade()
            .context("could not upgrade reference to file")?;
        file.add_export(name, var)
    }

    /// Store a variable to this function. Will get dropped when the function goes out of scope.
    pub(crate) fn register_variable(&self, name: Cow<'static, str>, var: Primitive) -> Result<()> {
        // let call_stack = rc_to_ref(&self.call_stack);
        self.call_stack.borrow_mut().register_variable(name, var)
    }

    pub(crate) fn ref_variable(&self, name: Cow<'static, str>, var: PrimitiveFlagsPair) {
        self.call_stack.borrow_mut().ref_variable(name, var)
    }

    /// Store a variable to the top of the call stack *only*. Will get dropped when the stack frame goes out of scope.
    pub(crate) fn register_variable_local(&self, name: String, var: Primitive) -> Result<()> {
        self.call_stack
            .borrow_mut()
            .register_variable_local(name, var, VariableFlags::none())
    }

    pub(crate) fn delete_variable_local(&self, name: &str) -> Result<PrimitiveFlagsPair> {
        self.call_stack.borrow_mut().delete_variable_local(name)
    }

    /// Get the [`Primitive`] value and its associated [`VariableFlags`] from a name.
    ///
    /// Will start the search in the current function and bubble all the way up to the highest stack frame.
    pub(crate) fn load_variable(&self, name: &str) -> Option<PrimitiveFlagsPair> {
        self.call_stack.borrow().find_name(name)
    }

    pub(crate) fn load_self_export(&self, name: &str) -> Option<PrimitiveFlagsPair> {
        let file = self
            .function
            .location()
            .upgrade()
            .expect("could not upgrade reference to file");

        file.get_export(name)
    }

    /// Get a reference to all of the variables mapped to this function.
    pub(crate) fn get_frame_variables(&self) -> Result<Ref<VariableMapping>> {
        let Ok(frame_variables) =
            Ref::filter_map(self.call_stack.borrow(), |x| x.get_frame_variables().ok())
        else {
            bail!(
                "could not get frame variables (stack = {:?})",
                self.call_stack
            )
        };

        Ok(frame_variables)
    }

    /// Get the [`Primitive`] value and its associated [`VariableFlags`] from a name.
    ///
    /// Will search _exclusively_ in the current stack frame, which makes this function more performant
    /// when searching for variables that should exist in the same stack frame.
    pub(crate) fn load_local(&self, name: &str) -> Result<PrimitiveFlagsPair> {
        self.call_stack
            .borrow()
            .get_frame_variables()
            .context("cannot load variables")?
            .get(name)
            .context("name not found")
    }

    /// Return the callback variables associated to this function, if they exist.
    pub(crate) fn get_callback_variables(&self) -> Option<Rc<VariableMapping>> {
        self.callback_state.as_ref().cloned()
    }
}
