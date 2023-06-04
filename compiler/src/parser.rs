use std::borrow::Cow;
use std::cell::Ref;
use std::{cell::RefCell, rc::Rc};

use anyhow::{Context, Error, Result};
use pest_consume::Parser as ParserDerive;

use crate::ast::{Compile, CompiledFunctionId, CompiledItem, Declaration, Ident, TypeLayout};
use crate::instruction;
use crate::scope::{Scope, ScopeType};

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
    errors: RefCell<Vec<Error>>,
}

impl AssocFileData {
    pub fn new(ty: ScopeType, destination_name: String) -> Self {
        let source_name = if destination_name.ends_with(".mmm") {
            let mut owned = destination_name[..destination_name.len() - 3].to_owned();
            owned.push_str("ms");
            owned
        } else {
            panic!("destination must be .mmm")
        };

        Self {
            scopes: RefCell::new(vec![Scope::new(ty)]),
            file_name: Rc::new(destination_name), // last_arg_type: Rc::new(RefCell::new(vec![]))
            source_name: Rc::new(source_name),
            errors: RefCell::new(vec![]),
        }
    }

    pub fn get_errors(&self) -> Ref<Vec<Error>> {
        self.errors.borrow()
    }

    pub fn get_nth_error(&self, idx: usize) -> Option<&Error> {
        unsafe { (*self.errors.as_ptr()).get(idx) }
    }

    pub fn add_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }

    pub fn get_source_file_name(&self) -> Rc<String> {
        self.source_name.clone()
    }

    pub fn get_file_name(&self) -> Rc<String> {
        self.file_name.clone()
    }

    pub fn get_return_type(&self) -> &Option<Cow<'static, TypeLayout>> {
        unsafe {
            (*self.scopes.as_ptr())
                .last()
                .expect("no scope registered")
                .peek_yields_value()
        }
    }

    pub fn push_scope_typed(&self, ty: ScopeType, yields: Option<Cow<'static, TypeLayout>>) {
        self.scopes
            .borrow_mut()
            .push(Scope::new_with_yields(ty, yields));
    }

    pub fn pop_scope(&self) -> Option<Cow<'static, TypeLayout>> {
        self.scopes
            .borrow_mut()
            .pop()
            .expect("pop without scope")
            .get_yields_value()
    }

    pub fn add_dependency(&self, dependency: &Ident) -> Result<()> {
        self.scopes
            .borrow_mut()
            .last_mut()
            .context("no scopes registered")?
            .add_dependency(dependency)
    }

    pub fn get_dependency_flags_from_name(&self, dependency: String) -> Option<(&Ident, bool)> {
        let iter = unsafe { (*self.scopes.as_ptr()).iter() };

        let mut is_callback = false;

        for scope in iter.rev() {
            if let Some(flags) = scope.contains(&dependency) {
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

    pub fn parse_with_userdata<'i, D>(
        rule: Rule,
        input_str: &'i str,
        user_data: D,
    ) -> Result<Nodes<'i, Rule, D>, pest_consume::Error<Rule>> {
        <Parser as pest_consume::Parser>::parse_with_userdata(rule, input_str, user_data)
    }

    pub fn parse<'i>(
        rule: Rule,
        input_str: &'i str,
    ) -> Result<Nodes<'i, Rule, ()>, pest_consume::Error<Rule>> {
        <Parser as pest_consume::Parser>::parse(rule, input_str)
    }
}

pub(crate) fn root_node_from_str(input_str: &str, user_data: Rc<AssocFileData>) -> Result<Node> {
    let source_name = &*user_data.get_source_file_name();
    let x = util::parse_with_userdata(Rule::file, input_str, user_data).map_err(|err| {
        err.with_path(source_name).renamed_rules(|rule| match rule {
            Rule::math_expr => "expression".to_owned(),
            Rule::function_return_type => "return type".to_owned(),
            rule => format!("{rule:?}"),
        })
    });

    let x = x?.single()?;
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

impl Compile for File {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let mut global_scope_code: Vec<CompiledItem> = self
            .declarations
            .iter()
            .flat_map(|x| x.compile(function_buffer).unwrap())
            .collect();

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
    pub fn file<'a>(input: Node) -> Result<File> {
        let mut result = File::default();

        for child in input.children() {
            match child.as_rule() {
                Rule::declaration => {
                    if let Some(d) = Self::declaration(child) {
                        result.add_declaration(d);
                    }
                }
                Rule::EOI => (),
                _ => unreachable!("{child:?}"),
            }
        }

        Ok(result)
    }
}
