use std::borrow::Cow;
use std::cell::{Ref, RefCell, Cell};
use std::collections::HashMap;
use std::path::{Path, PathBuf, Display};
use std::rc::Rc;
use std::sync::Arc;

use anyhow::{anyhow, bail, Result};
use pest_consume::Parser as ParserDerive;

use crate::ast::{
    ClassType, CompilationState, Compile, CompiledFunctionId, CompiledItem, Declaration,
    Dependencies, Export, FunctionParameters, Ident, IntoType, ModuleType, TypeLayout,
};
use crate::scope::{
    Scope, ScopeHandle, ScopeIter, ScopeReturnStatus, ScopeType, Scopes, TypeSearchResult,
};
use crate::{
    instruction, perform_file_io_in, root_ast_from_str, BytecodePathStr, CompilationError,
    FileManager, VecErr,
};

#[allow(unused)]
pub(crate) type Node<'i> = pest_consume::Node<'i, Rule, Rc<AssocFileData>>;

/// The AST parser for MScript. Various `impl` blocks define how each source code tree node should operate.
#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub(crate) struct Parser;

#[derive(Debug)]
pub(crate) struct AssocFileData {
    scopes: Scopes,
    file_name: Arc<PathBuf>,
    source_name: Arc<PathBuf>,
    exports: RefCell<Option<Export>>,
    files: FileManager,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ImportResult {
    module: ModuleType,
    was_cached: bool,
    compilation_lock: CompilationLock,
}

impl ImportResult {
    #[doc(hidden)]
    pub fn new(module: ModuleType, was_cached: bool, compilation_lock: CompilationLock) -> Self {
        Self {
            module,
            was_cached,
            compilation_lock,
        }
    }
    pub fn module(&self) -> ModuleType {
        self.module.clone()
    }

    pub const fn cached(&self) -> bool {
        self.was_cached
    }

    pub fn compilation_lock(&self) -> CompilationLock {
        self.compilation_lock.clone()
    }
}

impl AssocFileData {
    pub fn new(destination_unknown: impl AsRef<Path>, files_loaded: FileManager) -> Self {
        let destination = destination_unknown.as_ref();

        let dst_file_ext = destination
            .extension()
            .expect("no file extension")
            .bytecode_str();

        assert_eq!(dst_file_ext, "mmm", "destination must be .mmm");

        Self {
            scopes: Scopes::new(),
            file_name: Arc::new(destination.to_path_buf()),
            source_name: Arc::new(destination.with_extension("ms").to_path_buf()),
            files: files_loaded,
            exports: RefCell::default(),
        }
    }

    pub fn file_manager(&self) -> &FileManager {
        &self.files
    }

    // pub fn get_module_type(
    //     &self,
    //     path: Arc<PathBuf>,
    // ) -> Result<Ref<ModuleType>, Vec<anyhow::Error>> {
    //     if let Some(cached) = self.files.get_module_type(&path) {
    //         return Ok(cached);
    //     }

    //     let source = self.import_io(&path)?;
    //     let with_ext = path.with_extension("mmm");

    //     let root = root_ast_from_str(&*path, with_ext, &source, self.files.clone())?;
        
    //     let new_file = root.user_data()
    //             .files
    //             .register_ast(path.clone(), File::new_with_location(path.clone()))
    //             .details(
    //                 root.as_span(),
    //                 &root.user_data().get_source_file_name(),
    //                 "Circular dependency graph detected at compile time!",
    //             )
    //             .to_err_vec()?
    //             .clone();

    //     root.user_data().begin_module(new_file.get_export_ref());

    //     let module_type = ModuleType::from_node(&root)?;

    //     Ok(self.files.register_module(path, module_type))
    // }

    pub fn get_source_file_name(&self) -> String {
        self.source_name.bytecode_str()
    }

    #[allow(unused)]
    pub fn source_path(&self) -> Arc<PathBuf> {
        self.source_name.clone()
    }

    pub fn get_file_name(&self) -> String {
        self.file_name.bytecode_str()
    }

    pub fn bytecode_path(&self) -> Arc<PathBuf> {
        self.file_name.clone()
    }

    pub fn scopes_since_loop(&self) -> Result<usize> {
        let mut result = 1;

        for scope in self.scopes.iter() {
            if scope.is_loop() {
                return Ok(result);
            }

            if scope.is_function() {
                break;
            }

            result += 1;
        }

        bail!("no loop found")
    }

    pub fn register_function_parameters_to_scope(
        &self,
        parameters: Rc<FunctionParameters>,
    ) -> Option<Rc<FunctionParameters>> {
        self.scopes.register_function_parameters(parameters)
    }

    pub fn was_path_preloaded(&self, path: impl AsRef<Path>) -> bool {
        self.files.preloaded_file_exists(&path.bytecode_str())
    }

    fn import_io(&self, path: &Path) -> Result<Cow<'static, str>, Vec<anyhow::Error>> {
        Ok(
            if let Some(code) = self.files.get_preloaded_file(&path.bytecode_str()) {
                Cow::Borrowed(code)
            } else {
                Cow::Owned(perform_file_io_in(path.as_ref()).to_err_vec()?)
            },
        )
    }

    pub fn import(&self, path: Arc<PathBuf>) -> Result<ImportResult, Vec<anyhow::Error>> {
        if let Some(result) = self.files.get_module_type(&path) {
            return Ok(result.to_owned());
        }

        // if let Some(result) = self.files.get_incomplete_module(&path) {
        //     log::info!("using incomplete module: {:?}", result.module());
        //     return Ok(result);
        // }

        log::debug!("Cache miss on {path:?}");

        // else, cache miss

        let source = self.import_io(&path)?;
        let with_ext = path.with_extension("mmm");

        let root = root_ast_from_str(&*path, &with_ext, &source, self.files.clone())?;

        let new_file = root.user_data().files.register_ast(path.clone(), File::new_with_location(Arc::new(with_ext))).to_err_vec()?;
        root.user_data().begin_module(new_file.get_export_ref(), new_file.get_compilation_lock());

        let module_type = ModuleType::from_node(&root)?;

        log::info!("+ mod {path:?} {module_type:?}");

        let module = self
            .files
            .register_module(path, ImportResult::new(module_type, false, new_file.get_compilation_lock()))
            .to_owned();

        assert_eq!(&root.user_data().files, &self.files);

        Ok(module)

        // Parser::file(root)
    }

    pub fn is_at_module_level(&self) -> bool {
        self.scopes.depth() == 1
    }

    pub fn get_type_from_str(&self, ty: &str) -> TypeSearchResult {
        self.scopes.get_type_from_str(ty)
    }

    pub fn get_return_type(&self) -> Ref<ScopeReturnStatus> {
        Ref::map(self.scopes.last(), Scope::peek_yields_value)
    }

    pub fn push_if_typed(&self, yields: ScopeReturnStatus) -> ScopeHandle {
        self.push_scope_typed(ScopeType::IfBlock, yields)
    }

    pub fn push_else_typed(&self, yields: ScopeReturnStatus) -> ScopeHandle {
        self.push_scope_typed(ScopeType::ElseBlock, yields)
    }

    pub fn push_function(&self, yields: ScopeReturnStatus) -> ScopeHandle {
        self.push_scope_typed(ScopeType::Function(None), yields)
    }

    pub fn push_while_loop(&self, yields: ScopeReturnStatus) -> ScopeHandle {
        self.push_scope_typed(ScopeType::WhileLoop, yields)
    }

    pub fn push_number_loop(&self, yields: ScopeReturnStatus) -> ScopeHandle {
        self.push_scope_typed(ScopeType::NumberLoop, yields)
    }

    pub fn push_class_unknown_self(&self) -> ScopeHandle {
        self.push_scope_typed(ScopeType::Class(None), ScopeReturnStatus::No)
    }

    pub fn set_self_type_of_class(&self, new_class_type: ClassType) {
        self.scopes.set_self_type_of_class(new_class_type)
    }

    pub fn get_owned_type_of_executing_class(&self) -> Option<ClassType> {
        self.scopes.get_owned_type_of_executing_class(0)
    }

    pub fn get_type_of_executing_class(&self) -> Option<Ref<ClassType>> {
        self.scopes.get_type_of_executing_class(0)
    }

    pub fn get_type_of_executing_class_in_nth_frame(
        &self,
        skip_n_frames: usize,
    ) -> Option<Ref<ClassType>> {
        self.scopes.get_type_of_executing_class(skip_n_frames)
    }

    pub(crate) fn begin_module(&self, using_exports: Export, compilation_lock: CompilationLock) {
        let mut module_view = self.file_manager().loaded_modules.borrow_mut();
        let entry = module_view.entry(self.source_path()).or_insert_with(|| {
            let exported_members = using_exports.exports.upgrade().unwrap();
            let public_types = using_exports.public_types.upgrade().unwrap();
            let module_type = ModuleType::new_initialized(self.source_path(), exported_members, public_types);

            ImportResult::new(module_type, false, compilation_lock)
        });

        *self.exports.borrow_mut() = Some(using_exports)
    }

    // pub fn get_backing_export_struct(&self) -> Option<Arc<RefCell<Vec<Ident>>>> {
    //     let mut window = self.exports.borrow_mut();
    //     window.take()
    // }

    // pub fn get_backing_public_types_struct(
    //     &self,
    // ) -> Option<Arc<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>> {
    //     let mut window = self.public_types.borrow_mut();
    //     window.take()
    // }

    pub fn get_export_ref_for(&self, path: &PathBuf) -> Option<(Ref<File>, Export)> {
        {
            let debug_view = self.files.completed_ast.borrow();
            let x = debug_view.values().map(|x| &x.location).map(|x| x.display().to_string()).collect::<Vec<_>>();
            log::info!("loading exports for {} (existing export refs: {x:?})", path.display());
        }

        let file = self.files.get_ast_file(path)?;

        let exports = file.get_export_ref();

        Some((file, exports))
    }

    pub fn get_export_ref(&self) -> Export {
        self.exports
            .borrow()
            .as_ref()
            .expect(&format!("`exports` has not been set up for {self:?}"))
            .to_owned()

        // self
        // let window = self.exports.borrow();
        // let public_types = self.public_types.borrow();

        // if let (Some(window), Some(public_types)) = (window.as_ref(), public_types.as_ref()) {
        //     Export {
        //         exports: Arc::downgrade(window),
        //         public_types: Arc::downgrade(public_types),
        //     }
        // } else {
        //     drop(window);
        //     drop(public_types);

        //     let exports = Arc::default();
        //     let public_types = Arc::default();

        //     let exports_weak_ref = Arc::downgrade(&exports);
        //     let public_types_weak_ref = Arc::downgrade(&public_types);

        //     {
        //         let mut window = self.exports.borrow_mut();
        //         *window = Some(exports);
        //         *self.public_types.borrow_mut() = Some(public_types);
        //     }

        //     let export = Export {
        //         exports: exports_weak_ref,
        //         public_types: public_types_weak_ref,
        //     };

        //     export
        // }
    }

    pub fn get_current_executing_function(
        &self,
    ) -> Result<(Ref<Scope>, Ref<Rc<FunctionParameters>>)> {
        let iter = self.scopes.iter();

        for scope in iter {
            if scope.is_function() {
                let (scope, parameters) = Ref::map_split(scope, |scope| {
                    let ScopeType::Function(parameters) = scope.ty_ref() else {
                        unreachable!();
                    };

                    (scope, parameters)
                });

                let Ok(parameters) = Ref::filter_map(parameters, |parameters| {
                    let Some(parameters) = parameters.as_ref() else {
                        if let Some(dependency) = scope.contains("self") {
                            if dependency.ty().expect("unmapped type").as_ref()
                                == &TypeLayout::ClassSelf
                            {
                                return None;
                            }
                        }

                        unreachable!("function parameters should have been initialized");
                    };

                    Some(parameters)
                }) else {
                    bail!("in a class, `self` cannot be used to recursively call a member function because `self` is a reference to the class and not the function.")
                };

                return Ok((scope, parameters));
            }
        }

        bail!("no function scope encountered yet")
    }

    pub fn return_statement_expected_yield_type(&self) -> Option<Ref<Cow<'static, TypeLayout>>> {
        for scope in self.scopes.iter() {
            let Ok(result) = Ref::filter_map(scope, |x| x.peek_yields_value().get_type()) else {
                continue;
            };

            return Some(result);
        }

        None
    }

    pub fn mark_should_return_as_completed(&self) {
        self.scopes.mark_should_return_as_completed();
    }

    /// Returns the depth at which the stack is expected to be once the added frame is cleaned up.
    pub fn push_scope_typed(&self, ty: ScopeType, yields: ScopeReturnStatus) -> ScopeHandle {
        let depth = {
            self.scopes.push_scope_typed(ty, yields);
            self.scopes.depth()
        };

        ScopeHandle::new(depth, &self.scopes)
    }

    pub fn did_scope_exit_with_value_if_required(&self) -> bool {
        let last = self.scopes.last();
        let yields = last.peek_yields_value();

        !matches!(yields, ScopeReturnStatus::Should(..))
    }

    pub fn add_dependency(&self, dependency: &Ident) {
        self.scopes.add_variable(dependency)
    }

    pub fn add_type(&self, name: Box<str>, ty: Cow<'static, TypeLayout>) {
        self.scopes.add_type(name, ty)
    }

    pub fn has_name_been_mapped(&self, dependency: &str) -> bool {
        self.get_dependency_flags_from_name(dependency).is_some()
    }

    pub fn has_name_been_mapped_local(&self, dependency: &str) -> bool {
        self.get_ident_from_name_local(dependency).is_some()
    }

    pub fn get_ident_from_name_local(&self, dependency: &str) -> Option<Ref<Ident>> {
        let scope = self.scopes.last();

        Ref::filter_map(scope, |scope| scope.contains(dependency)).ok()
    }

    pub fn is_function_a_class_method(&self) -> bool {
        let mut iter = self.scopes.iter();

        let this_frame = iter.next().unwrap();
        let parent_scope = iter.next().unwrap();

        this_frame.is_class() || parent_scope.is_class()
    }

    pub fn get_dependency_flags_from_name(&self, dependency: &str) -> Option<(Ref<Ident>, bool)> {
        let scopes = self.scopes.iter();
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, 0)
    }

    pub fn get_dependency_flags_from_name_skip_n(
        &self,
        dependency: &str,
        skip: usize,
    ) -> Option<(Ref<Ident>, bool)> {
        let scopes = self.scopes.iter();
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, skip)
    }

    fn get_dependency_flags_from_name_and_scopes_plus_skip<'a>(
        &'a self,
        dependency: &str,
        scopes: ScopeIter<'a>,
        skip: usize,
    ) -> Option<(Ref<Ident>, bool)> {
        let mut is_callback = false;

        for (count, scope) in scopes.enumerate() {
            if let (true, Ok(flags)) = (
                count >= skip,
                Ref::filter_map(Ref::clone(&scope), |scope| scope.contains(dependency)),
            ) {
                return Some((flags, is_callback));
            }

            // should come after we check the contents of a scope.
            if scope.is_function() {
                is_callback = true;
            }
        }

        None
    }
}

#[allow(dead_code)]
pub(crate) mod util {
    use std::rc::Rc;

    use pest_consume::Nodes;

    use super::{AssocFileData, Parser, Rule};

    macro_rules! rename {
        ($($rule:ident => $text:literal $(,)?)*) => {
            |rule| match rule {
                $(
                    Rule::$rule => ($text).to_owned(),
                )*
                rule => format!("{rule:?}"),
            }
        }
    }

    pub(crate) fn parse_with_userdata_features(
        rule: Rule,
        input_str: &str,
        user_data: Rc<AssocFileData>,
    ) -> Result<Nodes<Rule, Rc<AssocFileData>>, Box<pest_consume::Error<Rule>>> {
        parse_with_userdata(rule, input_str, user_data.clone()).map_err(|error| {
            Box::new(
                error
                    .with_path(&user_data.source_name.to_string_lossy())
                    .renamed_rules(rename! {
                        math_expr => "expression",
                        function_return_type => "return type",
                        function_type => "function type",
                        open_ended_type => "open ended list spread",
                        list_type => "list type",
                        list_type_open_only => "list spread",
                        add => "+",
                        subtract => "-",
                        multiply => "*",
                        divide => "/",
                        modulo => "%",
                        add_assign => "+=",
                        sub_assign => "-=",
                        mul_assign => "*=",
                        div_assign => "/=",
                        mod_assign => "%=",
                        lt => "<",
                        gt => ">",
                        lte => "<=",
                        gte => ">=",
                        eq => "==",
                        neq => "!=",
                        and => "&&",
                        or => "||",
                        xor => "^",
                        assignment_flag => "assignment flag",
                        assignment_unpack => "assignment unpack",
                        dot_chain => "dot chain",
                        class_constructor => "constructor",
                        class_bound_function => "member function",
                        class_flag => "class flag",
                    }),
            )
        })
    }

    pub(crate) fn parse_with_userdata<D>(
        rule: Rule,
        input_str: &str,
        user_data: D,
    ) -> Result<Nodes<Rule, D>, Box<pest_consume::Error<Rule>>> {
        <Parser as pest_consume::Parser>::parse_with_userdata(rule, input_str, user_data)
            .map_err(Box::new)
    }
}

pub(crate) fn root_node_from_str(input_str: &str, user_data: Rc<AssocFileData>) -> Result<Node> {
    let x = util::parse_with_userdata_features(Rule::file, input_str, user_data);

    x.and_then(|x| x.single().map_err(Box::new))
        .map_err(|e| anyhow!(e))
}

#[derive(Debug, Default, Clone, PartialEq)]
pub(crate) struct CompilationLock(Arc<Cell<bool>>);

impl CompilationLock {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mark_compiled(&self) {
        self.0.set(true);
    }

    pub fn can_compile(&self) -> bool {
        !self.0.get()
    }
}

impl std::fmt::Display for CompilationLock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Can Compile: {}", self.can_compile())
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct File {
    pub declarations: Arc<RefCell<Vec<Declaration>>>,
    pub location: Arc<PathBuf>,
    pub exports: Arc<RefCell<Vec<Ident>>>,
    pub public_types: Arc<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>,
    pub compiled: CompilationLock,
}

impl File {
    pub fn new_with_location(location: Arc<PathBuf>) -> Self {
        Self {
            location,
            ..Default::default()
        }
    }

    fn add_declaration(&mut self, declaration: Declaration) {
        let mut view = self.declarations.borrow_mut();
        view.push(declaration);
    }

    pub(crate) fn get_compilation_lock(&self) -> CompilationLock {
        self.compiled.clone()
    }

    #[doc(hidden)]
    pub(crate) fn get_export_ref(&self) -> Export {
        Export {
            exports: Arc::downgrade(&self.exports),
            public_types: Arc::downgrade(&self.public_types),
        }
    }
}

impl IntoType for File {
    fn for_type(&self) -> Result<TypeLayout> {
        let module_type = ModuleType::new_initialized(
            self.location.clone(),
            self.exports.clone(),
            self.public_types.clone(),
        );

        Ok(TypeLayout::Module(module_type))
    }
}

impl Dependencies for File {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        let mut result = vec![];
        let view = unsafe { &*self.declarations.as_ptr() };

        for declaration in view.iter() {
            result.append(&mut declaration.net_dependencies());
        }

        result
    }
}

impl Compile<Vec<anyhow::Error>> for File {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
        let mut global_scope_code = vec![];

        let mut errors = vec![];

        let view = self.declarations.borrow();

        for declaration in view.iter() {
            match declaration.compile(state) {
                Ok(mut compiled) => global_scope_code.append(&mut compiled),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        global_scope_code.push(instruction!(ret_mod));

        let main_function = CompiledItem::Function {
            id: CompiledFunctionId::Custom("__module__".to_owned()),
            content: Some(global_scope_code),
            location: self.location.clone(),
        };

        state.push_function(main_function);

        Ok(vec![])
    }
}

#[pest_consume::parser]
impl Parser {}

impl Parser {
    pub fn file(input: Node) -> Result<ImportResult, Vec<anyhow::Error>> {
        log::debug!("pre-walk {:?}", input.user_data().source_path());

        let mut result = File::new_with_location(input.user_data().bytecode_path());

        input.user_data().begin_module(result.get_export_ref(), result.get_compilation_lock());

        // if let Some(import_result) = input
        //     .user_data()
        //     .files
        //     .get_module_type(&input.user_data().source_path())
        // {
        //     // if import_result.cached() {
        //     return Ok(import_result.to_owned());
        //     // }
        // }


        // let mut result = if let Some(x) = input
        //     .user_data()
        //     .files
        //     .get_ast_file(&input.user_data().bytecode_path())
        // {
        //     log::warn!("already walked {:?}", x.location);
        //     x.to_owned()
        // } else {
        //     File::new_with_location(input.user_data().bytecode_path())
        // };

        // if import_self.will_compile() {
        input
            .user_data()
            .files
            .register_ast(input.user_data().source_path(), result.clone())
            .details(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "Circular dependency graph detected at compile time!!!",
            )
            .to_err_vec()?;
        //


        let module_type = ModuleType::from_node(&input)?;

        let path_for_preload_import = input.user_data().source_path().bytecode_str();

        log::info!("+ mod {path_for_preload_import:?} {module_type:?}");

        let import_self = input
            .user_data()
            .files
            .register_module(input.user_data().source_path(), ImportResult::new(module_type, false, result.get_compilation_lock()))
            .to_owned();

        log::info!("+ finished preload of {:?}", import_self.module().name());

        // if import_self.will_compile() {
            
        // }

        let mut errors = vec![];

        for child in input.children() {
            match child.as_rule() {
                Rule::declaration => match Self::declaration(child) {
                    Ok(d) => result.add_declaration(d),
                    Err(mut e) => errors.append(&mut e),
                },
                Rule::EOI => (),
                _ => unreachable!("{child:?}"),
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            // if let Some(exports) = input.user_data().get_backing_export_struct() {
            //     result.exports = exports;
            // };

            // if let Some(public_types) = input.user_data().get_backing_public_types_struct() {
            //     result.public_types = public_types;
            // }

            

            Ok(import_self)
        }
    }
}
