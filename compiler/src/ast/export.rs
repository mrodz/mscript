use std::{cell::RefCell, sync::Weak};

use anyhow::{Result, Context};

use crate::{
    instruction,
    parser::{Node, Parser},
    CompilationError, VecErr,
};

use super::{new_err, Compile, Dependencies, Ident};

#[derive(Debug)]
pub(crate) struct Export {
    pub(crate) exports: Weak<RefCell<Vec<Ident>>>,
}

impl Export {
    pub fn add(&mut self, ident: Ident) {
        let exports = self.exports.upgrade().expect("[ADD] backing export reference was dropped");

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
        let exports = self.exports.upgrade().expect("[COMPILE] backing export reference was dropped");

        let view = exports.borrow();

        let mut result = Vec::with_capacity(view.len());

        for export in view.iter() {
            result.push(instruction!(export_name(export.name())));
        }

        Ok(result)
    }
}

impl Parser {
    pub fn export(input: Node) -> Result<Export, Vec<anyhow::Error>> {
        if !input.user_data().is_at_module_level() {
            return Err(vec![new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "`export` statements must be placed at the topmost module level".to_owned(),
            )]);
        }

        let mut result = input.user_data().get_export_ref().to_err_vec()?;

        let mut errors = vec![];

        let user_data = input.user_data().as_ref();

        for export in input.children() {
            let ident_span = export.as_span();
            let mut ident = Self::ident(export).to_err_vec()?;

            if let Err(error) = ident.link_from_pointed_type_with_lookup(user_data).details(
                ident_span,
                &user_data.get_source_file_name(),
                "This cannot be exported",
            ) {
                errors.push(error);
            } else {
                result.add(ident);
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(result)
    }
}
