use std::{
    borrow::{Borrow, Cow},
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use anyhow::{bail, Result};

use crate::ast::{
    FunctionParameters, Ident, TypeLayout, BIGINT_TYPE, BOOL_TYPE, BYTE_TYPE, FLOAT_TYPE, INT_TYPE,
    STR_TYPE, ClassType,
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub(crate) enum ScopeType {
    File,
    Function(Option<Arc<FunctionParameters>>),
    IfBlock,
    ElseBlock,
    WhileLoop,
    NumberLoop,
    Class(ClassType),
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
            ScopeType::Class(class_type) => write!(f, "Class {}", class_type.name()),
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

#[derive(Clone, Debug)]
pub(crate) enum TypeSearchResult<'a> {
    Ok(SuccessTypeSearchResult<'a>),
    NotFound,
}

#[derive(Debug)]
pub(crate) enum SuccessTypeSearchResult<'a> {
    Primitive(&'static TypeLayout),
    Owned(TypeLayout),
    InScope(Ref<'a, TypeLayout>),
}

impl TypeSearchResult<'_> {
    #[allow(unused)]
    pub fn try_unwrap(&self) -> Result<&SuccessTypeSearchResult> {
        if let Self::Ok(success) = self {
            return Ok(success);
        }

        bail!("type not found")
    }
}

impl Clone for SuccessTypeSearchResult<'_> {
    fn clone(&self) -> Self {
        match self {
            Self::Primitive(x) => Self::Primitive(x),
            Self::Owned(x) => Self::Owned(x.clone()),
            Self::InScope(x) => Self::Owned((*x).clone()),
        }
    }
}

impl SuccessTypeSearchResult<'_> {
    pub fn into_cow(self) -> Cow<'static, TypeLayout> {
        match self {
            Self::Primitive(x) => Cow::Borrowed(x),
            Self::Owned(x) => Cow::Owned(x),
            Self::InScope(x) => Cow::Owned(x.clone()),
        }
    }

    #[allow(unused)]
    pub fn unwrap(self) -> TypeLayout {
        match self {
            Self::Primitive(x) => x.to_owned(),
            Self::Owned(x) => x,
            Self::InScope(x) => x.clone(),
        }
    }
}

impl<'a> std::ops::Deref for TypeSearchResult<'a> {
    type Target = SuccessTypeSearchResult<'a>;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Ok(x) => x,
            Self::NotFound => panic!("cannot deref NotFound"),
        }
    }
}

impl<'a> std::ops::Deref for SuccessTypeSearchResult<'a> {
    type Target = TypeLayout;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::InScope(x) => x,
            Self::Owned(x) => x,
            Self::Primitive(x) => x.borrow(),
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

    #[allow(unused)]
    pub(crate) fn add_type(&self, name: Box<str>, ty: TypeLayout) {
        self.last_mut().add_type(name, ty);
    }

    pub(crate) fn get_type_from_str(&self, str: &str) -> TypeSearchResult {
        use SuccessTypeSearchResult::*;
        match str {
            "int" => return TypeSearchResult::Ok(Primitive(&INT_TYPE)),
            "str" => return TypeSearchResult::Ok(Primitive(&STR_TYPE)),
            "float" => return TypeSearchResult::Ok(Primitive(&FLOAT_TYPE)),
            "bool" => return TypeSearchResult::Ok(Primitive(&BOOL_TYPE)),
            "bigint" => return TypeSearchResult::Ok(Primitive(&BIGINT_TYPE)),
            "byte" => return TypeSearchResult::Ok(Primitive(&BYTE_TYPE)),
            _ => (),
        }

        for scope in self.iter() {
            let maybe_ty = Ref::filter_map(scope, |scope| {
                if let Some(ty) = scope.get_type(str) {
                    return Some(ty);
                }
                None
            })
            .ok();

            if let Some(ty) = maybe_ty {
                return TypeSearchResult::Ok(InScope(ty));
            }
        }

        TypeSearchResult::NotFound
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

impl<'a> ScopeHandle<'a> {
    pub(crate) fn new(depth_at_init: usize, belongs_to: &'a Scopes) -> Self {
        Self {
            depth_at_init,
            parent: belongs_to,
            consumed: false,
        }
    }

    pub(crate) fn consume(mut self) -> ScopeReturnStatus {
        self.consumed = true;

        self.parent
            .pop()
            .expect("pop without scope")
            .get_yields_value()
    }
}

impl Drop for ScopeHandle<'_> {
    fn drop(&mut self) {
        if self.consumed {
            return;
        }

        let scopes_in_parent = { self.parent.0.borrow().len() };

        if self.depth_at_init == scopes_in_parent {
            self.parent.pop();
            return;
        }

        panic!("discontinuity in virtual compilation stack: expected depth of {}, but the current depth is {}. This probably means a buggy AST element popped some frames incorrectly.\n{}\n", scopes_in_parent, self.depth_at_init, self.parent);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    types: HashMap<Box<str>, TypeLayout>,
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
    fn new(
        variables: HashSet<Ident>,
        types: HashMap<Box<str>, TypeLayout>,
        ty: ScopeType,
        yields: ScopeReturnStatus,
    ) -> Self {
        Self {
            variables,
            types,
            ty,
            yields,
        }
    }

    pub fn new_file() -> Self {
        Self::new_with_ty_yields(ScopeType::File, ScopeReturnStatus::No)
    }

    pub fn new_with_ty_yields(ty: ScopeType, yields: ScopeReturnStatus) -> Self {
        Self::new(HashSet::new(), HashMap::new(), ty, yields)
    }

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

    pub fn is_class(&self) -> bool {
        matches!(self.ty, ScopeType::Class(..))
    }

    pub fn is_loop(&self) -> bool {
        matches!(self.ty, ScopeType::NumberLoop | ScopeType::WhileLoop)
    }

    fn add_dependency(&mut self, dependency: &Ident) {
        log::trace!("+ {dependency}");

        self.variables.insert(dependency.clone());
    }

    #[allow(unused)]
    fn add_type(&mut self, name: Box<str>, ty: TypeLayout) {
        self.types.insert(name, ty);
    }

    fn get_type(&self, value: &str) -> Option<&TypeLayout> {
        self.types.get(value)
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
