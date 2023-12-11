use std::{cell::RefCell, sync::Weak};

use anyhow::Result;

use crate::instruction;

use super::{Compile, Dependencies, Ident};

#[derive(Debug)]
pub(crate) struct Export {
    pub(crate) exports: Weak<RefCell<Vec<Ident>>>,
}

impl Export {
    pub fn add(&mut self, ident: Ident) {
        // if ident.ty().expect("ident without type").is_class() {
        //     log::trace!("skipping export of {ident}, because it exports itself");
        //     // every class exports itself
        //     return;
        // }

        let exports = self
            .exports
            .upgrade()
            .expect("[ADD] backing export reference was dropped");

        let mut view = exports.borrow_mut();

        view.push(ident);
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
