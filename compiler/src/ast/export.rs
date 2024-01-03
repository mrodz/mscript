use std::{borrow::Cow, cell::RefCell, sync::{Weak, Arc}, collections::HashMap, fmt::Display};

use anyhow::Result;

use crate::instruction;

use super::{Compile, Dependencies, Ident, TypeLayout};

#[derive(Debug, Clone)]
pub(crate) struct Export {
    pub(crate) exports: Weak<RefCell<Vec<Ident>>>,
    pub(crate) public_types: Weak<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>,
}

impl Display for Export {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exports = self.exports.upgrade().expect("exported values were dropped");
        let public_types = self.public_types.upgrade().expect("types were dropped");

        let exports = exports.borrow();
        let public_types = public_types.borrow();

        write!(f, "Export -- {exports:?} & {public_types:?}")
    }
}

impl Export {
    pub fn new_untied(exports: Arc<RefCell<Vec<Ident>>>, public_types: Arc<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>) -> Self {
        Self {
            exports: Arc::downgrade(&exports),
            public_types: Arc::downgrade(&public_types),
        }
    }

    pub fn add(&mut self, ident: Ident) {
        let exports = self
            .exports
            .upgrade()
            .expect("[ADD] backing export reference was dropped");

        let mut view = exports.borrow_mut();

        view.push(ident);
    }

    pub fn add_type(&mut self, name: String, ty: Cow<'static, TypeLayout>) {
        let types = self
            .public_types
            .upgrade()
            .expect("[ADD_TYPE] backing type export reference was dropped");

        let mut view = types.borrow_mut();

        view.insert(name, ty);
    }
}

impl Dependencies for Export {}

impl Compile for Export {
    fn compile(
        &self,
        _: &super::CompilationState,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let exports = self
            .exports
            .upgrade()
            .expect("[COMPILE] backing export reference was dropped");

        let view = exports.borrow();

        let mut result = Vec::with_capacity(view.len());

        for export in view.iter() {
            if !export.ty().expect("ident without type").is_class() {
                result.push(instruction!(export_name(export.name())));
            } else {
                // classes are already exported by default; no double export
                log::trace!("Skipping export of {export} because it already exports itself");
            }
        }

        Ok(result)
    }
}
