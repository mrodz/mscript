use std::{
    borrow::{Borrow, Cow},
    cell::{Ref, RefCell},
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use anyhow::{bail, Result};

use crate::ast::{ClassType, FunctionParameters, Ident, StrWrapper, TypeLayout};

#[derive(Debug, Clone, PartialEq, Hash)]
pub(crate) enum ScopeType {
    File,
    Function(Option<Rc<FunctionParameters>>),
    IfBlock,
    ElseBlock,
    WhileLoop,
    NumberLoop,
    Class(Option<ClassType>),
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
            ScopeType::Class(class_type) => write!(
                f,
                "Class {}",
                class_type
                    .as_ref()
                    .map_or_else(|| "<UNKNOWN>", |x| x.name())
            ),
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
    Owned(Cow<'static, TypeLayout>),
    InScope(Ref<'a, Cow<'static, TypeLayout>>),
}

impl Clone for SuccessTypeSearchResult<'_> {
    fn clone(&self) -> Self {
        match self {
            Self::Owned(x) => Self::Owned(x.clone()),
            Self::InScope(x) => Self::Owned((*x).clone()),
        }
    }
}

impl SuccessTypeSearchResult<'_> {
    pub fn into_cow(self) -> Cow<'static, TypeLayout> {
        match self {
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

    pub(crate) fn wipe_top(&self) {
        let mut x = self.0.borrow_mut();
        x.last_mut().unwrap().wipe_variables()
    }

    pub(crate) fn mark_should_return_as_completed(&self) -> bool {
        let mut x = self.0.borrow_mut();
        x.last_mut().unwrap().mark_should_return_as_completed()
    }

    pub(crate) fn set_self_type_of_class(&self, new_class_type: ClassType) {
        let mut x = self.0.borrow_mut();

        let last = x.last_mut().unwrap();

        assert!(last.is_class());

        for field in new_class_type.fields() {
            let ty = field.ty().unwrap();
            if let Some(function) = ty.is_function() {
                let (is_return_type_class_self, is_return_type_optional) = {
                    // scoped because `try_set_return_type` borrows mutably
                    let return_type = function.return_type();
                    let Some(ty) = return_type.get_type() else {
                        continue;
                    };
                    (
                        ty.disregard_distractors(true).is_class_self(),
                        ty.disregard_distractors(false).is_optional().0,
                    )
                };

                if is_return_type_class_self {
                    let new_class_type = new_class_type.clone();

                    let mut return_type = Cow::Owned(TypeLayout::Class(new_class_type));

                    if is_return_type_optional {
                        return_type = Cow::Owned(TypeLayout::Optional(Some(Box::new(return_type))))
                    }

                    let new_return_status = ScopeReturnStatus::Did(return_type);

                    function.try_set_return_type(new_return_status)
                }
            }
        }

        let ScopeType::Class(ref mut class_type) = last.ty else {
            unreachable!()
        };

        *class_type = Some(new_class_type);
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
        { self.0.borrow_mut() }.pop()
    }

    pub(crate) fn add_variable(&self, dependency: &Ident) {
        let mut x = self.0.borrow_mut();
        x.last_mut().unwrap().add_dependency(dependency);
    }

    pub(crate) fn add_type(&self, name: Box<str>, ty: Cow<'static, TypeLayout>) {
        let mut x = self.0.borrow_mut();

        let scope = x.last_mut().unwrap();

        scope.add_type(name, ty);
    }

    pub(crate) fn register_function_parameters(
        &self,
        parameters: Rc<FunctionParameters>,
    ) -> Option<Rc<FunctionParameters>> {
        let mut view = self.0.borrow_mut();
        for scope in view.iter_mut().rev() {
            if let ScopeType::Function(ref mut function) = scope.ty {
                let result = function.take();
                *function = Some(parameters);
                return result;
            }
        }
        None
    }

    pub(crate) fn get_type_from_str(&self, str: &str) -> TypeSearchResult {
        use crate::ast::NativeType::*;
        use SuccessTypeSearchResult::*;

        match str {
            "int" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(Int)))),
            "str" => {
                return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(Str(
                    StrWrapper::unknown_size(),
                )))))
            }
            "float" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(Float)))),
            "bool" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(Bool)))),
            "bigint" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(BigInt)))),
            "byte" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::Native(Byte)))),
            "Self" => return TypeSearchResult::Ok(Owned(Cow::Owned(TypeLayout::ClassSelf))),
            _ => (),
        }

        for scope in self.iter() {
            let maybe_ty = Ref::filter_map(scope, |scope| {
                if let Some(tuple) = scope.get_type(str) {
                    return Some(tuple);
                }
                None
            })
            .ok();

            if let Some(borrow) = maybe_ty {
                return TypeSearchResult::Ok(InScope(borrow));
            }
        }

        TypeSearchResult::NotFound
    }

    pub(crate) fn get_owned_type_of_executing_class(
        &self,
        step_n_frames: usize,
    ) -> Option<ClassType> {
        let scopes = self.0.borrow();
        let iter = scopes.iter().rev().skip(step_n_frames);

        for scope in iter {
            if let ScopeType::Class(Some(ref class_type)) = scope.ty {
                return Some(class_type.to_owned());
            };
        }

        None
    }

    pub(crate) fn get_type_of_executing_class(
        &self,
        step_n_frames: usize,
    ) -> Option<Ref<ClassType>> {
        Ref::filter_map(self.0.borrow(), |scopes| {
            let iter = scopes.iter().rev().skip(step_n_frames);

            for scope in iter {
                if let ScopeType::Class(Some(ref class_type)) = scope.ty {
                    return Some(class_type);
                };
            }

            None
        })
        .ok()
    }
}

impl Display for Scopes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "== Virtual Stack:")?;
        for item in self.iter() {
            write!(f, "\n   {item}")?;
        }

        write!(f, "\n==")
    }
}

pub(crate) struct ScopeHandle<'a> {
    depth_at_init: usize,
    parent: &'a Scopes,
    consumed: bool,
}

impl<'a> ScopeHandle<'a> {
    pub(crate) const fn new(depth_at_init: usize, belongs_to: &'a Scopes) -> Self {
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

        use crate::ast::TypecheckFlags;
        Ok(lhs.eq_complex(rhs, &TypecheckFlags::<&ClassType>::classless()))
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
    types: HashMap<Box<str>, Cow<'static, TypeLayout>>,
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
    const fn new(
        variables: HashSet<Ident>,
        types: HashMap<Box<str>, Cow<'static, TypeLayout>>,
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

        // #167: We must use `.replace()` because `Ident` is hashed by its name, NOT its type.
        // This causes a reassignment to not register the updated type.

        if let Some(prev) = self.variables.replace(dependency.clone()) {
            log::debug!(
                "++ {} used to have type {}, but now it has type {}",
                prev.name(),
                prev.ty().unwrap(),
                dependency.ty().unwrap()
            )
        }
    }

    fn add_type(&mut self, name: Box<str>, ty: Cow<'static, TypeLayout>) {
        log::trace!("in scope {}, adding type {name} = {ty}", self.ty);
        self.types.insert(name, ty);
    }

    fn get_type(&self, value: &str) -> Option<&Cow<'static, TypeLayout>> {
        self.types.get(value)
    }

    pub fn ty_ref(&self) -> &ScopeType {
        &self.ty
    }

    pub fn mark_should_return_as_completed(&mut self) -> bool {
        self.yields.mark_should_return_as_completed().is_ok()
    }

    pub fn wipe_variables(&mut self) {
        self.variables.clear()
    }

    /// able to be improved
    pub fn contains(&self, dependency: &str) -> Option<&Ident> {
        // Ident hashes names exclusively, so we can pass `ty = None`
        self.variables.iter().find(|&x| x.name() == dependency)
    }
}
