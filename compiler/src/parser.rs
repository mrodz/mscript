use std::borrow::Cow;
use std::{cell::RefCell, rc::Rc};

use anyhow::{anyhow, Result, bail};
use pest_consume::Parser as ParserDerive;

use crate::ast::{Compile, CompiledFunctionId, CompiledItem, Declaration, Ident, TypeLayout};
use crate::instruction;
use crate::scope::{Scope, ScopeReturnStatus, ScopeType};

#[allow(unused)]
pub(crate) type Node<'i> = pest_consume::Node<'i, Rule, Rc<AssocFileData>>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub(crate) struct Parser;

#[derive(Debug)]
pub(crate) struct AssocFileData {
    scopes: RefCell<Vec<Scope>>,
    file_name: Rc<String>, // last_arg_type: Rc<RefCell<Vec<IdentType>>>
    source_name: Rc<String>,
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
            scopes: RefCell::new(vec![Scope::new_file()]),
            file_name: Rc::new(destination_name), // last_arg_type: Rc::new(RefCell::new(vec![]))
            source_name: Rc::new(source_name),
        }
    }

    pub fn get_source_file_name(&self) -> Rc<String> {
        self.source_name.clone()
    }

    pub fn get_file_name(&self) -> Rc<String> {
        self.file_name.clone()
    }

    pub fn scopes_since_loop(&self) -> Result<usize> {
        let mut result = 1;

        for scope in self.scopes.borrow().iter().rev() {
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

    #[allow(clippy::mut_from_ref)]
    pub fn get_return_type(&self) -> &mut ScopeReturnStatus {
        unsafe {
            (*self.scopes.as_ptr())
                .last_mut()
                .expect("no scope registered")
                .peek_yields_value_mut()
        }
    }

    pub fn push_if_typed(&self, yields: ScopeReturnStatus) {
        self.push_scope_typed(ScopeType::IfBlock, yields)
    }

    pub fn push_else_typed(&self, yields: ScopeReturnStatus) {
        self.push_scope_typed(ScopeType::ElseBlock, yields)
    }

    pub fn push_function(&self, yields: ScopeReturnStatus) {
        self.push_scope_typed(ScopeType::Function, yields)
    }

    pub fn push_while_loop(&self, yields: ScopeReturnStatus) {
        self.push_scope_typed(ScopeType::WhileLoop, yields)
    }

    pub fn push_number_loop(&self, yields: ScopeReturnStatus) {
        self.push_scope_typed(ScopeType::NumberLoop, yields)
    }

    pub fn return_statement_expected_yield_type(&self) -> Option<&Cow<'static, TypeLayout>> {
        for scope in unsafe { (*self.scopes.as_ptr()).iter().rev() } {
            let (ScopeReturnStatus::Should(result) | ScopeReturnStatus::Did(result)) = scope.peek_yields_value() else {
                continue;
            };

            return Some(result);
        }

        None
    }

    pub fn push_scope_typed(&self, ty: ScopeType, yields: ScopeReturnStatus) {
        self.scopes
            .borrow_mut()
            .push(Scope::new_with_ty_yields(ty, yields));
    }

    pub fn did_scope_exit_with_value_if_required(&self) -> bool {
        let scopes = self.scopes.borrow();
        let yields = scopes.last().expect("no scope").peek_yields_value();

        !matches!(yields, ScopeReturnStatus::Should(..))
    }

    pub fn pop_scope(&self) -> ScopeReturnStatus {
        self.scopes
            .borrow_mut()
            .pop()
            .expect("pop without scope")
            .get_yields_value()
    }

    pub fn add_dependency(&self, dependency: &Ident) {
        self.scopes
            .borrow_mut()
            .last_mut()
            .expect("no scopes registered")
            .add_dependency(dependency);
    }

    pub fn has_name_been_mapped(&self, dependency: &String) -> bool {
        self.get_dependency_flags_from_name(dependency).is_some()
    }

    pub fn has_name_been_mapped_local(&self, dependency: &String) -> bool {
        self.get_ident_from_name_local(dependency).is_some()
    }

    pub fn get_ident_from_name_local(&self, dependency: &String) -> Option<&Ident> {
        let scope = unsafe { (*self.scopes.as_ptr()).last()? };

        scope.contains(dependency)
    }

    pub fn get_dependency_flags_from_name(&self, dependency: &String) -> Option<(&Ident, bool)> {
        let scopes = unsafe { (*self.scopes.as_ptr()).iter() };
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, 0)
    }

    pub fn get_dependency_flags_from_name_skip_n(
        &self,
        dependency: &String,
        skip: usize,
    ) -> Option<(&Ident, bool)> {
        let scopes = unsafe { (*self.scopes.as_ptr()).iter() };
        self.get_dependency_flags_from_name_and_scopes_plus_skip(dependency, scopes, skip)
    }

    fn get_dependency_flags_from_name_and_scopes_plus_skip<'a, S>(
        &'a self,
        dependency: &String,
        scopes: S,
        skip: usize,
    ) -> Option<(&Ident, bool)>
    where
        S: Iterator<Item = &'a Scope> + DoubleEndedIterator,
    {
        let mut is_callback = false;

        // let mut c = 0;

        for (count, scope) in scopes.rev().enumerate() {
            // println!("\t\t{:?}", scope.scope_type());
            if let (true, Some(flags)) = (count >= skip, scope.contains(dependency)) {
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
pub mod util {
    use pest_consume::Nodes;

    use super::{Parser, Rule};

    pub fn parse_with_userdata<D>(
        rule: Rule,
        input_str: &str,
        user_data: D,
    ) -> Result<Nodes<Rule, D>, Box<pest_consume::Error<Rule>>> {
        <Parser as pest_consume::Parser>::parse_with_userdata(rule, input_str, user_data)
            .map_err(Box::new)
    }

    pub fn parse(
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
    let source_name = &*user_data.get_source_file_name();
    let x = util::parse_with_userdata(Rule::file, input_str, user_data).map_err(|err| {
        err.with_path(source_name).renamed_rules(|rule| match rule {
            Rule::math_expr => "expression".to_owned(),
            Rule::function_return_type => "return type".to_owned(),
            rule => format!("{rule:?}"),
        })
    });

    let x = x
        .map_err(|e| vec![anyhow!(e)])?
        .single()
        .map_err(|e| vec![anyhow!(e)])?;

    Ok(x)
}

#[derive(Debug, Default)]
pub(crate) struct File {
    pub declarations: Vec<Declaration>,
    pub location: Rc<String>,
}

impl File {
    fn add_declaration(&mut self, declaration: Declaration) {
        self.declarations.push(declaration)
    }
}

impl Compile<Vec<anyhow::Error>> for File {
    fn compile(
        &self,
        function_buffer: &mut Vec<CompiledItem>,
    ) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
        let mut global_scope_code = vec![];

        let mut errors = vec![];

        for declaration in &self.declarations {
            match declaration.compile(function_buffer) {
                Ok(mut compiled) => global_scope_code.append(&mut compiled),
                Err(e) => errors.push(e),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        global_scope_code.push(instruction!(ret));

        let main_function = CompiledItem::Function {
            id: CompiledFunctionId::Custom("main".into()),
            content: Some(global_scope_code),
            location: self.location.clone(),
        };

        function_buffer.push(main_function);

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
