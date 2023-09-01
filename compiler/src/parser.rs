use std::borrow::Cow;
use std::cell::{Ref, RefMut};
use std::rc::Rc;
use std::sync::Arc;

use anyhow::{anyhow, bail, Result};
use pest_consume::Parser as ParserDerive;

use crate::ast::{
    CompilationState, Compile, CompiledFunctionId, CompiledItem, Declaration, Dependencies,
    FunctionParameters, Ident, TypeLayout, ClassType,
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
    /// Scopes is cloned ON EVERY SINGLE child node walk... uh oh BIG TIME!!!!!!!!!!!!!!!!!!!!!!!!!!!
    scopes: Scopes,
    file_name: Arc<String>, // last_arg_type: Rc<RefCell<Vec<IdentType>>>
    source_name: Arc<String>,
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
            file_name: Arc::new(destination_name), // last_arg_type: Rc::new(RefCell::new(vec![]))
            source_name: Arc::new(source_name),
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

    pub fn get_type_from_str(&self, ty: &str) -> TypeSearchResult {
        self.scopes.get_type_from_str(ty)
    }

    pub fn get_return_type_mut(&self) -> RefMut<ScopeReturnStatus> {
        RefMut::map(self.scopes.last_mut(), Scope::peek_yields_value_mut)
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

    pub fn push_class(&self, class_type: ClassType) -> ScopeHandle {
        self.push_scope_typed(ScopeType::Class(class_type), ScopeReturnStatus::No)
    }

    pub fn get_current_executing_function(
        &self,
    ) -> Option<(Ref<Scope>, Ref<Arc<FunctionParameters>>)> {
        let mut iter = self.scopes.iter();

        iter.find(|x| x.is_function()).map(|scope| {
            Ref::map_split(scope, |scope| {
                let ty = scope.ty_ref();

                let ScopeType::Function(parameters) = ty else {
                    unreachable!()
                };

                (
                    scope,
                    parameters
                        .as_ref()
                        .expect("function parameters should have been initialized"),
                )
            })
        })
    }

    // pub fn register_function_parameters_to_scope(
    //     &self,
    //     function_parameters: Arc<FunctionParameters>,
    // ) {
    //     let mut this_scope = self.scopes.last_mut();

    //     let idents = function_parameters.slice();

    //     for ident in idents {
    //         self.scopes.add_variable(ident);
    //     }

    //     this_scope.add_parameters(function_parameters);

    // }

    pub fn return_statement_expected_yield_type(&self) -> Option<Ref<Cow<'static, TypeLayout>>> {
        for scope in self.scopes.iter() {
            let Ok(result) = Ref::filter_map(scope, |x| x.peek_yields_value().get_type()) else {
                continue;
            };

            return Some(result);
            // let Some(ty) = scope.peek_yields_value().get_type();

            // save this code in case this change breaks it:
            // let (ScopeReturnStatus::Should(result) | ScopeReturnStatus::Did(result)) = scope.peek_yields_value() else {
            //     continue;
            // };
        }

        None
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

    pub fn has_name_been_mapped(&self, dependency: &String) -> bool {
        self.get_dependency_flags_from_name(dependency).is_some()
    }

    pub fn has_name_been_mapped_local(&self, dependency: &String) -> bool {
        self.get_ident_from_name_local(dependency).is_some()
    }

    pub fn get_ident_from_name_local(&self, dependency: &String) -> Option<Ref<Ident>> {
        let scope = self.scopes.last();

        Ref::filter_map(scope, |scope| scope.contains(dependency)).ok()
    }

    pub fn is_function_a_class_method(&self) -> bool {
        let mut iter = self.scopes.iter();

        // skip this stack frame
        iter.next().unwrap();

        let parent_scope = iter.next().unwrap();

        parent_scope.is_class()
    }

    pub fn get_dependency_flags_from_name(
        &self,
        dependency: &String,
    ) -> Option<(Ref<Ident>, bool)> {
        let scopes = self.scopes.iter();
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, 0)
    }

    pub fn get_dependency_flags_from_name_skip_n(
        &self,
        dependency: &String,
        skip: usize,
    ) -> Option<(Ref<Ident>, bool)> {
        let scopes = self.scopes.iter();
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, skip)
    }

    fn get_dependency_flags_from_name_and_scopes_plus_skip<'a>(
        &'a self,
        dependency: &String,
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
                        xor => "^"
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

    let x = x
        .map_err(|e| vec![anyhow!(e)])?
        .single()
        .map_err(|e| vec![anyhow!(e)])?;

    Ok(x)
}

#[derive(Debug, Default)]
pub(crate) struct File {
    pub declarations: Vec<Declaration>,
    pub location: Arc<String>,
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
            Ok(result)
        }
    }
}
