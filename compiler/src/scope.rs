use std::{collections::HashSet, borrow::Cow};

use anyhow::Result;

use crate::ast::{Ident, TypeLayout};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ScopeType {
    File,
    Function,
}

#[derive(Debug)]
pub(crate) struct Scope {
    variables: HashSet<Ident>,
    ty: ScopeType,
    yields: Option<Cow<'static, TypeLayout>>,
}

impl Scope {
    pub fn new(ty: ScopeType) -> Self {
        Self {
            variables: HashSet::new(),
            ty,
            yields: None,
        }
    }

    pub fn new_with_yields(ty: ScopeType, yields: Option<Cow<'static, TypeLayout>>) -> Self {
        Self {
            variables: HashSet::new(),
            ty,
            yields: yields,
        }
    }

    pub fn does_yield_value(&self) -> bool {
        self.yields.is_some()
    }

    pub fn peek_yields_value(&self) -> &Option<Cow<'static, TypeLayout>> {
        &self.yields
    }

    pub fn get_yields_value(self) -> Option<Cow<'static, TypeLayout>> {
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
