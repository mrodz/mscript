use std::{borrow::Cow, collections::HashSet};

use anyhow::{bail, Result};

use crate::ast::{Ident, TypeLayout};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ScopeType {
    File,
    Function,
    IfBlock,
    ElseBlock,
    WhileLoop,
    NumberLoop,
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
    pub fn eq_for_signature_checking(&self, rhs: &Self) -> Result<bool> {
        if self == rhs {
            return Ok(true);
        }

        if let Self::Should(left) | Self::Did(left) = self {
            if let Self::Should(right) | Self::Did(right) = rhs {
                return Ok(left == right);
            }
        }

        bail!("not applicable")
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
        self.ty == ScopeType::Function
    }

    pub fn add_dependency(&mut self, dependency: &Ident) {
        self.variables.insert(dependency.clone());
    }

    /// able to be improved
    pub fn contains(&self, dependency: &String) -> Option<&Ident> {
        // Ident hashes names exclusively, so we can pass `ty = None`
        self.variables.iter().find(|&x| x.name() == dependency)
    }
}
