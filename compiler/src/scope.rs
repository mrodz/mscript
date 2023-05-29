use std::collections::HashSet;

use anyhow::Result;

use crate::ast::Ident;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ScopeType {
    File,
    Function,
}

#[derive(Debug)]
pub(crate) struct Scope {
    variables: HashSet<Ident>,
    pub ty: ScopeType,
}

impl Scope {
    pub fn new(ty: ScopeType) -> Self {
        Self {
            variables: HashSet::new(),
            ty,
        }
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
