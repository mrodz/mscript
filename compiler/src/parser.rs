use std::{cell::RefCell, rc::Rc};

use anyhow::{Context, Result};
use pest_consume::Parser as ParserDerive;

use crate::{
    ast::{Compile, CompiledFunctionId, CompiledItem, Declaration, Ident, TypeLayout},
    instruction,
    scope::{Scope, ScopeType},
};

#[allow(unused)]
pub(crate) type Node<'i> = pest_consume::Node<'i, Rule, AssocFileData>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub(crate) struct Parser;

#[derive(Clone, Debug)]
pub(crate) struct AssocFileData {
    scopes: Rc<RefCell<Vec<Scope>>>,
    // last_arg_type: Rc<RefCell<Vec<IdentType>>>
}

impl AssocFileData {
    pub fn new(ty: ScopeType) -> Self {
        Self {
            scopes: Rc::new(RefCell::new(vec![Scope::new(ty)])),
            // last_arg_type: Rc::new(RefCell::new(vec![]))
        }
    }

    pub fn run_in_scope<R>(&self, ty: ScopeType, code: impl FnOnce() -> R) -> R {
        self.push_scope(ty);
        let result: R = code();
        self.pop_scope();
        result
    }

    pub fn push_scope(&self, ty: ScopeType) {
        let mut scopes = self.scopes.borrow_mut();
        scopes.push(Scope::new(ty));
    }

    pub fn pop_scope(&self) {
        let mut scopes = self.scopes.borrow_mut();
        scopes.pop();
    }

    pub fn add_dependency(&self, dependency: &Ident) -> Result<()> {
        let mut scopes = self.scopes.borrow_mut();
        scopes
            .last_mut()
            .context("no scopes registered")?
            .add_dependency(dependency)
    }

    pub fn get_dependency_flags_from_name(&self, dependency: String) -> Option<&Ident> {
        let iter = unsafe { (*self.scopes.as_ptr()).iter() };

        for scope in iter.rev() {
            if let Some(flags) = scope.contains(&dependency) {
                return Some(flags);
            }
        }

        None
    }
}

pub(crate) fn root_node_from_str(str: &str) -> Result<Node> {
    let x = <Parser as pest_consume::Parser>::parse_with_userdata(
        Rule::file,
        str,
        AssocFileData::new(ScopeType::File),
    )?
    .single()?;

    Ok(x)
}

#[derive(Debug, Default)]
pub(crate) struct File {
    pub declarations: Vec<Declaration>,
}

impl File {
    fn add_declaration(&mut self, declaration: Declaration) {
        self.declarations.push(declaration)
    }
}

impl Compile for File {
    fn compile(&self) -> Result<Vec<CompiledItem>> {
        let statements: Vec<CompiledItem> = self
            .declarations
            .iter()
            .flat_map(|x| x.compile().unwrap())
            // .flatten()
            .collect();

        let mut functions = vec![];
        let mut global_scope_code = vec![];

        for statement in statements {
            match statement {
                function @ CompiledItem::Function { .. } => {
                    functions.push(function);
                }
                instruction @ CompiledItem::Instruction { .. } => {
                    global_scope_code.push(instruction)
                }
            }
        }

        global_scope_code.push(instruction!(ret));

        functions.push(CompiledItem::Function {
            id: CompiledFunctionId::Custom("main".into()),
            content: global_scope_code,
        });

        // dbg!(&functions);

        for function in functions {
            println!("{}", function.repr(true))
        }

        todo!()

        // functions
    }
}

#[pest_consume::parser]
impl Parser {
    pub fn file<'a>(input: Node) -> Result<File> {
        let mut result = File::default();

        for child in input.children() {
            match child.as_rule() {
                Rule::declaration => {
                    let d = Self::declaration(child)?;
                    result.add_declaration(d);
                }
                Rule::EOI => (),
                _ => unreachable!("{child:?}"),
            }
        }

        Ok(result)
    }
}
