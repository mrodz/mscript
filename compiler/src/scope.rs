use std::{borrow::Cow, collections::HashSet};

use anyhow::Result;

use crate::ast::{Ident, TypeLayout};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ScopeType {
    File,
    Function,
    IfBlock,
    ElseBlock,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ScopeReturnStatus {
    NoReturn,
    VoidReturn,
    ShouldReturn(Cow<'static, TypeLayout>),
    DidReturn(Cow<'static, TypeLayout>),
}

impl ScopeReturnStatus {
    pub fn detect_should_return(val: Option<Cow<'static, TypeLayout>>) -> Self {
        if let Some(type_layout) = val {
            Self::ShouldReturn(type_layout)
        } else {
            Self::VoidReturn
        }
    }

    pub fn mark_should_return_as_completed(&mut self) -> Result<&mut Self> {
        if let Self::ShouldReturn(expected_return_type) | Self::DidReturn(expected_return_type) = self {
            let x = expected_return_type.to_owned();
            *self = ScopeReturnStatus::DidReturn(x);

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
        Self::new_with_ty_yields(ScopeType::File, ScopeReturnStatus::NoReturn)
    }

    pub fn new_with_ty_yields(ty: ScopeType, yields: ScopeReturnStatus) -> Self {
        Self { variables: HashSet::new(), ty, yields }
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

    pub fn add_dependency(&mut self, dependency: &Ident) -> Result<()> {
        self.variables.insert(dependency.clone());
        Ok(())
        // let ty = dependency.ty()?.clone();
        // if !self.variables.insert(dependency.clone()) {
        //     bail!("conflicting mapping")
        // } else {
        //     Ok(())
        // }
        // if self.variables.insert(dependency.name().clone(), (dependency, ty)).is_some() {
        // bail!("conflicting variable mapping")
        // } else {
        // Ok(())
        // }
    }

    /// able to be improved
    pub fn contains(&self, dependency: &String) -> Option<&Ident> {
        // Ident hashes names exclusively, so we can pass `ty = None`
        for x in self.variables.iter() {
            if x.name() == dependency {
                return Some(&x);
            }
        }

        None
    }
}
