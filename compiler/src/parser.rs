use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use std::sync::{Arc, RwLock};

use anyhow::{anyhow, bail, Result};
use pest_consume::Parser as ParserDerive;

use crate::ast::{
    ClassType, CompilationState, Compile, CompiledFunctionId, CompiledItem, Declaration,
    Dependencies, Export, FunctionParameters, Ident, IntoType, TypeLayout, ModuleType,
};
use crate::instruction;
use crate::scope::{
    Scope, ScopeHandle, ScopeIter, ScopeReturnStatus, ScopeType, Scopes, TypeSearchResult,
};

#[allow(unused)]
pub(crate) type Node<'i> = pest_consume::Node<'i, Rule, Rc<AssocFileData>>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub(crate) struct Parser;

#[derive(Debug)]
pub(crate) struct AssocFileData {
    scopes: Scopes,
    file_name: Arc<String>,
    source_name: Arc<String>,
    class_id_c: RwLock<usize>,
    exports: RefCell<Option<Arc<RefCell<Vec<Ident>>>>>,
}

impl AssocFileData {
    pub fn new(destination_name: String) -> Self {
        let source_name = if destination_name.ends_with(".mmm") {
            let mut owned = destination_name[..destination_name.len() - 3].to_owned();
            owned.push_str("ms");
            owned
        } else {
            panic!("destination must be .mmm")
        };

        Self {
            scopes: Scopes::new(),
            file_name: Arc::new(destination_name),
            source_name: Arc::new(source_name),
            class_id_c: RwLock::new(0),
            exports: RefCell::new(None),
        }
    }

    pub fn get_source_file_name(&self) -> Arc<String> {
        self.source_name.clone()
    }

    pub fn get_file_name(&self) -> Arc<String> {
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

    // pub fn get_name_of_executing_class_in_nth_frame(&self, skip_n_frames: usize) -> Result<&str> {
    //     self.scopes.get_name_of_executing_class(step_n_frames)
    // }

    /// This function should **ONLY** be called when creating the AST Node for a Class Type.
    pub fn request_class_id(&self) -> usize {
        let mut class_id = self.class_id_c.write().unwrap();
        let result = *class_id;
        *class_id += 1;
        result
    }

    pub fn get_backing_export_struct(&self) -> Option<Arc<RefCell<Vec<Ident>>>> {
        let mut window = self.exports.borrow_mut();
        let result = window.take();
        result
    }

    pub fn get_export_ref(&self) -> Result<Export> {
        let window = self.exports.borrow();

        if let Some(window) = window.as_ref() {
            Ok(Export {
                exports: Arc::downgrade(window),
            })
        } else {
            drop(window);

            let exports = Arc::new(RefCell::new(vec![]));

            let weak_ref = Arc::downgrade(&exports);

            {
                let mut window = self.exports.borrow_mut();
                *window = Some(exports);
            }

            let export = Export { exports: weak_ref };

            Ok(export)
        }
    }

    pub fn get_current_executing_function(
        &self,
    ) -> Result<(Ref<Scope>, Ref<Arc<FunctionParameters>>)> {
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

    pub fn add_type(&self, name: Box<str>, ty: TypeLayout) {
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

        // let mut c = 0;

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
                    .with_path(&user_data.source_name)
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
                        dot_chain => "dot chain",
                        class_constructor => "constructor",
                        class_bound_function => "member function",
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

    pub(crate) fn parse(
        rule: Rule,
        input_str: &str,
    ) -> Result<Nodes<Rule, ()>, Box<pest_consume::Error<Rule>>> {
        <Parser as pest_consume::Parser>::parse(rule, input_str).map_err(Box::new)
    }
}

pub(crate) fn root_node_from_str(
    input_str: &str,
    user_data: Rc<AssocFileData>,
) -> Result<Node, Vec<anyhow::Error>> {
    let x = util::parse_with_userdata_features(Rule::file, input_str, user_data);

    x.map_err(|e| vec![anyhow!(e)])?
        .single()
        .map_err(|e| vec![anyhow!(e)])
}

#[derive(Debug, Default)]
pub(crate) struct File {
    pub declarations: Vec<Declaration>,
    pub location: Arc<String>,
    pub exports: Arc<RefCell<Vec<Ident>>>,
}

impl IntoType for File {
    fn for_type(&self) -> Result<TypeLayout> {
        let exported_members= Arc::downgrade(&self.exports);
        Ok(TypeLayout::Module(ModuleType::new(exported_members, Arc::downgrade(&self.location))))
    }
}

impl File {
    fn add_declaration(&mut self, declaration: Declaration) {
        self.declarations.push(declaration)
    }
}

impl Dependencies for File {
    fn dependencies(&self) -> Vec<crate::ast::Dependency> {
        let mut result = vec![];
        for declaration in &self.declarations {
            result.append(&mut declaration.net_dependencies());
        }
        result
    }
}

impl Compile<Vec<anyhow::Error>> for File {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
        let mut global_scope_code = vec![];

        let mut errors = vec![];

        for declaration in &self.declarations {
            match declaration.compile(state) {
                Ok(mut compiled) => global_scope_code.append(&mut compiled),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        global_scope_code.push(instruction!(ret));

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
impl Parser {
    pub fn file<'a>(input: Node) -> Result<File, Vec<anyhow::Error>> {
        let mut result = File::default();

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
            if let Some(exports) = input.user_data().get_backing_export_struct() {
                result.exports = exports;
            };

            Ok(result)
        }
    }
}
