use std::{
    borrow::{Borrow, Cow},
    cell::{Ref, RefCell, RefMut},
    collections::HashSet,
    fmt::Display,
    sync::Arc,
};

use anyhow::{bail, Result};

use crate::ast::{FunctionParameters, Ident, TypeLayout};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ScopeType {
    File,
    Function(Option<Arc<FunctionParameters>>),
    IfBlock,
    ElseBlock,
    WhileLoop,
    NumberLoop,
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeType::File => write!(f, "Module"),
            ScopeType::ElseBlock => write!(f, "Else Block"),
            ScopeType::IfBlock => write!(f, "If Block"),
            ScopeType::Function(Some(x)) => write!(f, "fn({x})"),
            ScopeType::Function(None) => write!(f, "fn(???)"),
            ScopeType::NumberLoop => write!(f, "Number Loop"),
            ScopeType::WhileLoop => write!(f, "While Loop"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Scopes(RefCell<Vec<Scope>>);

pub(crate) struct ScopeIter<'a> {
    inner: Ref<'a, [Scope]>,
}

impl<'a> Iterator for ScopeIter<'a> {
    type Item = Ref<'a, Scope>;

    fn next(&mut self) -> Option<Self::Item> {
        // if there's nothing left in the slice
        let inner_len = self.inner.len();

        if inner_len == 0 {
            return None;
        }

        let item = Ref::map(Ref::clone(&self.inner), |inner| &inner[inner_len - 1]);

        // no data movement because we're dealing with slices
        self.inner = Ref::map(Ref::clone(&self.inner), |inner| &inner[..inner_len - 1]);

        Some(item)
    }
}

impl<'a> ScopeIter<'a> {
    pub fn new<V: Borrow<[Scope]>>(slice: Ref<'a, V>) -> Self {
        ScopeIter {
            inner: Ref::map(slice, |v| v.borrow()),
        }
    }
}

impl Scopes {
    pub(crate) fn new() -> Self {
        log::trace!("INIT Virtual Stack at MODULE");
        Self(RefCell::new(vec![Scope::new_file()]))
    }

    pub(crate) fn depth(&self) -> usize {
        self.0.borrow().len()
    }

    pub(crate) fn iter(&self) -> ScopeIter {
        ScopeIter::new(self.0.borrow())
    }

    pub(crate) fn last(&self) -> Ref<Scope> {
        Ref::map(self.0.borrow(), |scopes| {
            scopes.last().expect("scopes was empty")
        })
    }

    pub(crate) fn last_mut(&self) -> RefMut<Scope> {
        RefMut::map(self.0.borrow_mut(), |scopes| {
            scopes.last_mut().expect("scopes was empty")
        })
    }

    /// Returns the depth at which the stack is expected to be once the added frame is cleaned up.
    pub(crate) fn push_scope_typed(&self, ty: ScopeType, yields: ScopeReturnStatus) {
        log::trace!("Virtual Stack PUSH: {ty} -> {yields:?}");

        self.0
            .borrow_mut()
            .push(Scope::new_with_ty_yields(ty, yields));
    }

    fn pop(&self) -> Option<Scope> {
        log::trace!("Virtual Stack POP");
        self.0.borrow_mut().pop()
    }

    pub(crate) fn add_variable(&self, dependency: &Ident) {
        self.last_mut().add_dependency(dependency);
    }
}

impl Display for Scopes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "== Virtual Stack:")?;
        for item in self.iter() {
            write!(f, "\n   {item}")?;
        }

        write!(f, "\n==")
        // write!(f, "")
    }
}

pub(crate) struct ScopeHandle<'a> {
    depth_at_init: usize,
    parent: &'a Scopes,
    consumed: bool,
}

impl <'a>ScopeHandle<'a> {
    pub(crate) fn new(depth_at_init: usize, belongs_to: &'a Scopes) -> Self {
        Self {
            depth_at_init,
            parent: belongs_to,
            consumed: false,
        }
    }    

    pub(crate) fn consume(mut self) -> ScopeReturnStatus {
        self.consumed = true;

        self.parent.pop()
            .expect("pop without scope")
            .get_yields_value()
    }
}

impl Drop for ScopeHandle<'_> {
    fn drop(&mut self) {
        if self.consumed {
            return;
        }
        
        let scopes_in_parent = {
            self.parent.0.borrow().len()
        };

        if self.depth_at_init == scopes_in_parent {
            self.parent.pop();
            return;
        }

        panic!("discontinuity in virtual compilation stack: expected depth of {}, but the current depth is {}. This probably means a buggy AST element popped some frames incorrectly.\n{}\n", scopes_in_parent, self.depth_at_init, self.parent);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ScopeReturnStatus {
    No,
    Void,
    Should(Cow<'static, TypeLayout>),
    ParentShould(Cow<'static, TypeLayout>),
    Did(Cow<'static, TypeLayout>),
}

impl ScopeReturnStatus {
    pub fn get_type(&self) -> Option<&Cow<'static, TypeLayout>> {
        match self {
            Self::Did(x) | Self::ParentShould(x) | Self::Should(x) => Some(x),
            _ => None,
        }
    }

    pub fn eq_for_signature_checking(&self, rhs: &Self) -> Result<bool> {
        if self == rhs {
            return Ok(true);
        }

        let (Some(lhs), Some(rhs)) = (self.get_type(), rhs.get_type()) else {
            bail!("not applicable")
        };

        Ok(lhs == rhs)
    }

    pub fn detect_should_return(val: Option<Cow<'static, TypeLayout>>) -> Self {
        if let Some(type_layout) = val {
            Self::Should(type_layout)
        } else {
            Self::Void
        }
    }

    pub fn all_branches_return(&self) -> bool {
        matches!(self, Self::Did(..))
    }

    pub fn mark_should_return_as_completed(&mut self) -> Result<&mut Self> {
        if let Self::Should(expected_return_type)
        | Self::ParentShould(expected_return_type)
        | Self::Did(expected_return_type) = self
        {
            let x = expected_return_type.clone();
            *self = ScopeReturnStatus::Did(x);
        };

        Ok(self)
    }
}

#[derive(Debug)]
pub(crate) struct Scope {
    variables: HashSet<Ident>,
    ty: ScopeType,
    yields: ScopeReturnStatus,
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ty)?;

        if let Some(yield_type) = self.yields.get_type() {
            write!(f, " -> {yield_type}")?;
        }

        Ok(())
    }
}

impl Scope {
    pub fn new_file() -> Self {
        Self::new_with_ty_yields(ScopeType::File, ScopeReturnStatus::No)
    }

    pub fn new_with_ty_yields(ty: ScopeType, yields: ScopeReturnStatus) -> Self {
        Self {
            variables: HashSet::new(),
            ty,
            yields,
        }
    }

    // pub fn add_parameters(&mut self, parameters: Arc<FunctionParameters>) {
    //     let ScopeType::Function(ref mut parameters_option @ None) = self.ty else {
    //         unreachable!("either not a function, or parameters have already been set");
    //     };

    //     *parameters_option = Some(parameters)
    // }

    pub fn peek_yields_value(&self) -> &ScopeReturnStatus {
        &self.yields
    }

    pub fn peek_yields_value_mut(&mut self) -> &mut ScopeReturnStatus {
        &mut self.yields
    }

    pub fn get_yields_value(self) -> ScopeReturnStatus {
        self.yields
    }

    pub fn is_function(&self) -> bool {
        matches!(self.ty, ScopeType::Function(..))
    }

    pub fn is_loop(&self) -> bool {
        matches!(self.ty, ScopeType::NumberLoop | ScopeType::WhileLoop)
    }

    fn add_dependency(&mut self, dependency: &Ident) {
        log::trace!("+ {dependency}");
    
        self.variables.insert(dependency.clone());
    }

    pub fn ty_ref(&self) -> &ScopeType {
        &self.ty
    }

    // pub fn ty_ref_mut(&mut self) -> &mut ScopeType {
    //     &mut self.ty
    // }

    /// able to be improved
    pub fn contains(&self, dependency: &String) -> Option<&Ident> {
        // Ident hashes names exclusively, so we can pass `ty = None`
        self.variables.iter().find(|&x| x.name() == dependency)
    }
}
