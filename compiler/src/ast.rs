mod assertion;
mod assignment;
mod boolean;
mod callable;
mod class;
mod declaration;
mod dot_lookup;
mod export;
mod function;
mod function_arguments;
mod function_body;
mod function_parameters;
mod function_return_type;
mod ident;
mod if_statement;
mod import;
mod list;
mod loop_control_flow;
mod math_expr;
mod number;
mod number_loop;
mod print_statement;
mod reassignment;
mod r#return;
mod string;
mod r#type;
mod value;
mod while_loop;

pub(crate) use assertion::Assertion;
pub(crate) use assignment::Assignment;
pub(crate) use callable::Callable;
pub(crate) use class::{Class, ClassType};
pub(crate) use declaration::Declaration;
pub(crate) use export::Export;
pub(crate) use function::Function;
pub(crate) use function_arguments::FunctionArguments;
pub(crate) use function_body::Block;
pub(crate) use function_parameters::FunctionParameters;
pub(crate) use ident::Ident;
pub(crate) use if_statement::IfStatement;
pub(crate) use import::Import;
pub(crate) use list::List;
pub(crate) use loop_control_flow::{Break, Continue};
pub(crate) use math_expr::{Expr, Op as BinaryOperation};
pub(crate) use number::Number;
pub(crate) use number_loop::NumberLoop;
pub(crate) use print_statement::PrintStatement;
pub(crate) use r#return::ReturnStatement;
pub(crate) use r#type::{IntoType, ModuleType, NativeType, StrWrapper, TypeLayout};
pub(crate) use reassignment::Reassignment;
pub(crate) use value::{CompileTimeEvaluate, ConstexprEvaluation, Value};
pub(crate) use while_loop::WhileLoop;

use anyhow::{anyhow, bail, Context, Error, Result};
use bytecode::compilation_bridge::{raw_byte_instruction_to_string_representation, Instruction};
use pest::Span;
use std::path::PathBuf;
use std::rc::Rc;
use std::{
    borrow::Cow,
    cell::{Cell, RefCell, UnsafeCell},
    fmt::{Debug, Display},
    ops::SubAssign,
    sync::Arc,
};

use crate::parser::{File, Node};
use crate::{FileManager, VecErr};

use self::number_loop::NumberLoopRegister;

pub(crate) trait WalkForType {
    fn type_from_node(input: &Node) -> Result<Ident>;
}

/// A register for a variable to be used throughout various compilation
/// implementations to store intermediate values.
///
/// Constructing a [`TemporaryRegister`] directly is not advised, because you
/// have to manage the allocation yourself. Instead, use the API provided via
/// the [`CompilationState`] struct.
#[derive(Debug)]
pub struct TemporaryRegister {
    /// The underlying value that this register holds.
    id: usize,
    /// Whether the register has been manually freed.
    in_use: bool,
    /// An unsafe raw pointer to a [`usize`] that will be used to check
    /// for any register smashing when this instance is dropped.
    cleanup_pointer: Option<*mut usize>,
}

impl TemporaryRegister {
    #[doc(hidden)]
    const fn new_rand(id: usize, in_use: bool, cleanup_pointer: Option<*mut usize>) -> Self {
        Self {
            id,
            in_use,
            cleanup_pointer,
        }
    }

    /// Create a new ghost register, that is to say, a register that does not
    /// modify the underlying counter in any way.
    #[doc(hidden)]
    unsafe fn new_ghost_register(mock_count: usize) -> Self {
        log::trace!("reg. -G--m {mock_count}");
        Self::new_rand(mock_count, false, None)
    }

    /// Create a new [`TemporaryRegister`] with an id and a raw
    /// pointer to the backing counter.
    ///
    #[doc(hidden)]
    fn new(id: usize, cleanup_ptr: *mut usize) -> Self {
        log::trace!("reg. -n--- {id}");
        Self::new_rand(id, true, Some(cleanup_ptr))
    }

    /// Create a new [`TemporaryRegister`] that will not run any
    /// cleanup code when it goes out of scope. It must be manually dropped
    /// using [`TemporaryRegister::free`]
    #[doc(hidden)]
    fn new_require_explicit_drop(id: usize) -> Self {
        log::trace!("reg. -n-e- {id}");
        Self::new_rand(id, true, None)
    }

    /// Manually clean up a [`TemporaryRegister`], which will cause its
    /// cleanup code to run now instead of when the variable goes out of scope.
    #[doc(hidden)]
    fn free(mut self, current_count: usize) {
        if self.id != current_count {
            unreachable!("dropped out of order");
        }

        self.in_use = false;

        log::trace!("reg. F--e- {}", self.id);
    }
}

impl Display for TemporaryRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.id)
    }
}

impl Drop for TemporaryRegister {
    fn drop(&mut self) {
        if self.in_use {
            if let Some(cleanup_ptr) = self.cleanup_pointer {
                unsafe {
                    if self.id != *cleanup_ptr - 1 {
                        panic!("dropped out of order: {self} / {}", *cleanup_ptr)
                    }

                    (*cleanup_ptr).sub_assign(1);
                }
            } else {
                panic!("{self:?} leaked")
            }
        }
    }
}

/// Grouping of either:
/// - a **generated** id, typically reserved for anonymous functions
/// - a **specified** id, used for named functions, classes, etc.
#[derive(Debug, Clone)]
pub(crate) enum CompiledFunctionId {
    /// An id dealt out by the compiler sequentially.
    Generated(isize),
    /// A custom, user-specified ID.
    /// # Note
    /// Supplying an ASCII number as this variant's input is undefined behavior.
    Custom(String),
}

impl Display for CompiledFunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompiledFunctionId::Custom(string) => write!(f, "{string}"),
            CompiledFunctionId::Generated(id) => write!(f, "__fn{id}"),
        }
    }
}

impl From<CompiledItem> for Instruction {
    fn from(value: CompiledItem) -> Self {
        let CompiledItem::Instruction { id, arguments } = value else {
            panic!("not an instruction: {value:?}");
        };

        Instruction::new(id, arguments)
    }
}

/// The backbone of the compiler's output.
/// Every program goes from:
/// 1. Source
/// 2. AST (`[crate::parser::Node]`)
/// 3. [`CompiledItem`] (Not Optimized) <<
/// 4. [`CompiledItem`] (Optimized) <<
#[derive(Debug, Clone)]
pub(crate) enum CompiledItem {
    Function {
        id: CompiledFunctionId,
        content: Option<Vec<CompiledItem>>,
        location: Arc<PathBuf>,
    },
    Instruction {
        id: u8,
        arguments: Box<[String]>,
    },
    Break(usize),
    Continue(usize),
}

impl Display for CompiledItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Break(..) => write!(f, "!BREAK"),
            Self::Continue(..) => write!(f, "!CONTINUE"),
            Self::Instruction { id, arguments } => write!(
                f,
                "{:?} {arguments:?}",
                raw_byte_instruction_to_string_representation(*id)
            ),
            Self::Function { id, location, .. } => write!(f, "Function `{id}` -- {location:?}"),
        }
    }
}

impl CompiledItem {
    pub fn is_loop_instruction(&self) -> bool {
        const WHILE_LOOP: u8 = 0x31;
        matches!(self, Self::Instruction { id: WHILE_LOOP, .. })
    }

    pub fn is_done_instruction(&self) -> bool {
        const DONE: u8 = 0x27;
        const JMP_POP: u8 = 0x32;
        matches!(
            self,
            Self::Instruction {
                id: DONE | JMP_POP,
                ..
            }
        )
    }

    pub fn repr(&self, use_string_version: bool) -> Result<String> {
        fn fix_arg_if_needed(arg: &String) -> Result<Cow<String>> {
            let starts = arg.starts_with('\"');
            let ends = arg.ends_with('\"');

            if starts ^ ends {
                bail!("non-matching `\"` on arg {arg}")
            }

            let has_quotes = starts && ends;

            if !has_quotes && arg.contains(' ') {
                let mut combined = String::with_capacity(arg.len() + 2);
                combined.push('"');
                combined.push_str(arg);
                combined.push('"');
                Ok(Cow::Owned(combined))
            } else {
                Ok(Cow::Borrowed(arg))
            }
        }

        match self {
            Self::Function { id, content, .. } => {
                let Some(ref content) = content else {
                    bail!("this is a function symbol, not a compilable function. {self:?}");
                };

                let mut result = String::new();
                for item in content {
                    result += &item.repr(use_string_version)?;
                }

                let func_name = id.to_string();

                let (sep, f, e) = if use_string_version {
                    ('\n', "function", "end")
                } else {
                    ('\0', "f", "e")
                };

                Ok(format!("{f} {func_name}{sep}{result}{e}{sep}"))
            }
            Self::Instruction { id, arguments } => {
                let mut args = String::new();

                if arguments.len() >= 1 {
                    for arg in &arguments[..] {
                        args.push(' ');
                        let arg = fix_arg_if_needed(arg).unwrap();
                        args.push_str(arg.as_str());
                    }
                }

                if use_string_version {
                    Ok(format!(
                        "\t{}{args}\n",
                        raw_byte_instruction_to_string_representation(*id).unwrap()
                    ))
                } else {
                    Ok(format!("{}{}\0", *id as char, args))
                }
            }
            Self::Break(..) | Self::Continue(..) => {
                unreachable!("break/continue that was not fulfilled")
            }
        }
    }
}

#[macro_export]
macro_rules! instruction {
    ($name:tt $($arg:tt)*) => {{
        use $crate::ast::*;

        let id = bytecode::compilation_bridge::string_instruction_representation_to_byte(stringify!($name))
            .expect("instruction does not exist");

        let arguments = vec![
            $(
                $arg.to_string(),
            )*
        ];

        let arguments = arguments.into_boxed_slice();

        CompiledItem::Instruction { id: *id, arguments }

    }};
}

#[derive(Debug)]
pub(crate) struct CompilationStep {
    path: PathBuf,
    next: Option<Box<CompilationStep>>,
}

impl CompilationStep {
    pub fn new(file: PathBuf) -> Self {
        log::trace!("Added compilation step: {file:?}");
        Self {
            path: file,
            next: None,
        }
    }
}

pub(crate) struct CompilationState {
    function_buffer: UnsafeCell<Vec<CompiledItem>>,
    function_id_c: Cell<isize>,
    loop_register_c: Cell<usize>,
    temporary_register_c: Cell<usize>,
    compilation_queue: Rc<RefCell<Option<Box<CompilationStep>>>>,
}

impl CompilationState {
    pub fn new() -> Self {
        Self {
            function_buffer: UnsafeCell::new(Vec::with_capacity(1)),
            function_id_c: Cell::new(0),
            loop_register_c: Cell::new(0),
            temporary_register_c: Cell::new(0),
            compilation_queue: Rc::new(RefCell::new(None)),
        }
    }

    pub fn queue_compilation(&self, path: PathBuf) {
        let mut view = self.compilation_queue.borrow_mut();

        let mut result = CompilationStep::new(path);

        if let Some(moved) = view.take() {
            result.next = Some(moved);
        }

        *view = Some(Box::new(result));
    }

    pub fn compile_recursive(
        &self,
        file_manager: &FileManager,
        driver: &mut impl FnMut(&File, Vec<CompiledItem>) -> Result<()>,
    ) -> Result<(), Vec<anyhow::Error>> {
        {
            let completed = file_manager.completed_ast.borrow();
            log::debug!(
                "[cc] FINAL COMPILATION LIST: {:?}",
                completed.keys().map(|x| x.display()).collect::<Vec<_>>()
            );
        }

        self.compile_recursive_interior(file_manager, driver, 1)
    }

    pub fn compile_recursive_interior(
        &self,
        file_manager: &FileManager,
        driver: &mut impl FnMut(&File, Vec<CompiledItem>) -> Result<()>,
        depth: usize,
    ) -> Result<(), Vec<anyhow::Error>> {
        let mut view = self.compilation_queue.borrow_mut();

        let mut node @ Some(..) = view.take() else {
            return Ok(());
        };

        while let Some(h) = node {
            let mut state_for_file = CompilationState::new();

            let file = file_manager
                .get_ast_file(&h.path)
                .with_context(|| {
                    format!(
                        "`{:?}` has not completed. File manager:\n{file_manager})",
                        h.path
                    )
                })
                .to_err_vec()?;

            log::info!("[cc] {} {}", "@".repeat(depth), file.location.display());

            file.compile(&state_for_file)?;

            driver(&file, state_for_file.take_function_buffer()).to_err_vec()?;

            state_for_file.compile_recursive_interior(file_manager, driver, depth + 1)?;

            node = h.next;
        }

        Ok(())
    }

    pub fn poll_function_id(&self) -> CompiledFunctionId {
        let id = self.function_id_c.get();
        let result = CompiledFunctionId::Generated(id);

        self.function_id_c.set(id + 1);

        result
    }

    pub fn poll_loop_register(&self) -> NumberLoopRegister {
        let id = self.loop_register_c.get();
        self.loop_register_c.set(id + 1);
        NumberLoopRegister::Generated(id + 1)
    }

    pub fn free_loop_register(&self, register: NumberLoopRegister) {
        if let NumberLoopRegister::Generated(id) = register {
            assert!(self.loop_register_c.get() == id);

            self.loop_register_c.set(id - 1);
        }
    }

    pub fn poll_temporary_register(&self) -> TemporaryRegister {
        let c = self.temporary_register_c.get();
        let result: TemporaryRegister =
            TemporaryRegister::new(c, self.temporary_register_c.as_ptr());
        self.temporary_register_c.set(c + 1);
        result
    }

    #[allow(unused)]
    pub fn poll_temporary_register_explicit_drop(&self) -> TemporaryRegister {
        let c = self.temporary_register_c.get();
        let result: TemporaryRegister = TemporaryRegister::new_require_explicit_drop(c);
        self.temporary_register_c.set(c + 1);
        result
    }

    /// No AutoDrop, but advances the counter. Use in conjunction with `free_many`.
    pub unsafe fn poll_temporary_register_ghost(&self) -> TemporaryRegister {
        let c = self.temporary_register_c.get();
        let result: TemporaryRegister = TemporaryRegister::new_ghost_register(c);
        self.temporary_register_c.set(c + 1);
        result
    }

    pub fn free_temporary_register(&self, reg: TemporaryRegister) {
        let c = self.temporary_register_c.get();
        self.temporary_register_c.set(c - 1);
        reg.free(c - 1);
    }

    pub unsafe fn free_many_temporary_registers(&self, count: usize) {
        log::trace!("reg. F--em {count}");
        self.temporary_register_c
            .set(self.temporary_register_c.get() - count);
    }

    pub fn take_function_buffer(&mut self) -> Vec<CompiledItem> {
        std::mem::take(self.function_buffer.get_mut())
    }

    pub fn push_function(&self, compiled_function: CompiledItem) {
        assert!(
            matches!(compiled_function, CompiledItem::Function { .. }),
            "Attempting to push a non-function to the function buffer"
        );
        unsafe {
            let function_buffer = self.function_buffer.get();

            (*function_buffer).push(compiled_function);
        }
    }
}

pub(crate) trait Compile<E = anyhow::Error> {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, E>;
}

pub(crate) trait Optimize {}

#[derive(Clone, Debug)]
pub(crate) struct Dependency<'a> {
    pub ident: Cow<'a, Ident>,
    pub cycles_needed: usize,
}

impl PartialEq for Dependency<'_> {
    fn eq(&self, other: &Self) -> bool {
        let same_names: bool = self.ident.name() == other.ident.name();
        let same_types: bool = self.ident.ty().unwrap() == other.ident.ty().unwrap();

        same_names && same_types
    }
}

impl<'a> Dependency<'a> {
    pub fn name(&self) -> &str {
        self.ident.as_ref().name()
    }
    pub const fn new(ident: Cow<'a, Ident>) -> Self {
        Self {
            ident,
            cycles_needed: 0,
        }
    }
    pub fn increment_cycle(&mut self) {
        self.cycles_needed += 1;
    }

    /// For self = `Dependency(x)` and other = `Dependency(y)`, check if `x == y` or `x == CallbackVariable(y)`
    ///
    /// # Errors
    /// Will error if either dependency is typeless.
    pub fn eq_allow_callbacks(&self, other: &Self) -> Result<bool> {
        if self.ident.name() != other.ident.name() {
            return Ok(false);
        }

        let other_ty: &TypeLayout = other.ident.ty()?.as_ref();
        let self_ty: &TypeLayout = self.ident.ty()?.as_ref();

        if let TypeLayout::CallbackVariable(ptr_ty) = other_ty {
            if other.cycles_needed > 0 {
                return Ok(self_ty == ptr_ty.as_ref());
            }
        }

        Ok(self_ty == other_ty)
    }
}

impl Display for Dependency<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl<'a> From<&'a Ident> for Dependency<'a> {
    fn from(value: &'a Ident) -> Self {
        Self::new(Cow::Borrowed(value))
    }
}

/// Default behavior for [`Dependencies::net_dependencies`]. This function is only
/// separate for public visibility, which allows implementations to call it.
///
/// Formula:
/// `[dependencies] - [supplies] = [net]`
///
/// The algorithm for "satisfying" dependencies is as follows:
/// * If dependency.name != supplied.name, continue
/// * If dependency is a variable from a child scope, succeed if supplied.type == dependency.type
/// * If dependency is not a variable from a child scope, it will be propagated regardless of whether the types
///   match (required for cases where a variable from a parent scope is copied to a separate local variable with the same name).
/// * Succeed if supplied.type == dependency.type
pub(crate) fn get_net_dependencies(ast_item: &dyn Dependencies, is_scope: bool) -> Vec<Dependency> {
    let supplies = ast_item.supplies();
    let dependencies = ast_item.dependencies();

    let mut result: Vec<Dependency> = Vec::with_capacity(dependencies.len());

    // For now, this is O(n^2) :(
    'dependency_loop: for mut dependency in dependencies {
        for supplied in &supplies {
            if supplied
                .eq_allow_callbacks(&dependency)
                .expect("idents do not have types")
            {
                continue 'dependency_loop;
            }
        }

        if is_scope {
            dependency.increment_cycle();
        }

        result.push(dependency);
    }

    result
}

pub(crate) trait Dependencies {
    /// This method expresses all new variables and identities created by an AST node.
    /// Identities supplied by a parent can be consumed by child AST nodes, but not the
    /// other way around.
    ///
    /// # Example
    /// A function might have a parameter `input`, and define variables `sum` and `product`.
    /// Thus, a function's [`Dependencies::supplies`] implementation should return `vec![input, sum, product]`.
    fn supplies(&self) -> Vec<Dependency> {
        vec![]
    }

    /// This method expresses all outstanding variables needed by this function. Generally,
    /// if an AST node comes across an identity, it should add it as a dependency. This even
    /// applies to variables defined inside a scope--If you see it, add it as a dependency.
    ///
    /// # Example
    /// A function might have a parameter `input`, define variables `sum` and `product`, and
    /// returns `input * product + sum`. Thus, a function's [`Dependencies::dependencies`] implementation
    /// should return `vec![input, sum, product]`.
    fn dependencies(&self) -> Vec<Dependency> {
        vec![]
    }

    /// This function is used to calculate the outstanding dependencies by filtering out identities
    /// _needed_ from the identities _supplied_ by an AST node. If implementing an AST node that
    /// **DIRECTLY** creates a scope (For example, a block of code), this method should be overriden.
    ///
    /// [`get_net_dependencies(self, false)`](get_net_dependencies) is the default implementation.
    /// Override this function and pass `true` if dealing with an AST node that creates a scope.
    fn net_dependencies(&self) -> Vec<Dependency>
    where
        Self: Sized,
    {
        get_net_dependencies(self, false)
    }
}

pub fn new_err(span: Span, file_name: &str, message: String) -> Error {
    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;

    let (line, col) = span.start_pos().line_col();
    log::error!("{file_name}:{line}:{col} > {message} > {:?}", span.as_str());

    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(file_name);

    anyhow!(custom_error)
}

pub fn map_err<R, E>(
    value: impl Into<Result<R, E>>,
    span: Span,
    file_name: &str,
    message: String,
) -> Result<R>
where
    E: Display + Debug + Send + Sync + 'static,
{
    let x: Result<R, E> = value.into();

    if let Ok(x) = x {
        return Ok(x);
    }

    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(file_name);

    let always_err = unsafe { x.unwrap_err_unchecked() };

    Err(anyhow!(always_err).context(custom_error))
}

pub fn map_err_messages<R, C>(
    mut value: Result<R>,
    span: Span,
    file_name: &str,
    message: String,
    description_messages: impl FnOnce() -> Vec<C>,
) -> Result<R>
where
    C: Display + Send + Sync + 'static,
{
    if value.is_ok() {
        return value; // no unwrap
    }

    let description_messages = description_messages();
    for message in description_messages {
        value = value.context(message)
    }

    use pest::error::ErrorVariant::CustomError;
    use pest_consume::Error as PE;
    let custom_error = PE::<()>::new_from_span(CustomError { message }, span).with_path(file_name);

    value.context(anyhow!(custom_error))
}
