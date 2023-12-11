use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};

use crate::{
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    BytecodePathStr, VecErr,
};

use super::{new_err, Compile, Dependencies, Ident, TypeLayout};

#[derive(Debug)]
pub(crate) enum Import {
    Standard {
        path: PathBuf,
        store: Ident,
        cached: bool,
    },
}

impl Compile for Import {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
        match self {
            Self::Standard {
                path,
                store,
                cached,
            } => {
                log::debug!("@IMPORT -- using cached: {cached}");

                if !cached {
                    log::debug!("queuing compilation of {path:?}");

                    state.queue_compilation(path.clone());
                }

                let module_loader =
                    format!("{}#__module__", path.with_extension("mmm").bytecode_str());
                Ok(vec![
                    instruction!(module_entry module_loader),
                    instruction!(store(store.name())),
                ])
            }
        }
    }
}

impl Dependencies for Import {}

impl Import {
    pub fn path_from_parts(user_data: &AssocFileData, str_part: &str) -> Result<PathBuf> {
        let src: &str = &user_data.get_source_file_name();
        let path = Path::new(src);
        let attempted_path = Path::new(str_part);
        let path = path.parent().context("no parent")?.join(attempted_path);
        Ok(path)
    }
}

impl Parser {
    pub fn import_path(input: Node) -> Result<PathBuf> {
        let path = Import::path_from_parts(input.user_data(), input.as_str())?;

        let with_extension = path.with_extension("ms");

        if input.user_data().was_path_preloaded(&with_extension) {
            return Ok(with_extension);
        }

        let path_exists = path.try_exists()?;

        if with_extension.try_exists()? {
            return Ok(with_extension.to_path_buf());
        }

        if path_exists && path.is_dir() {
            return Err(new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "This is a directory, and not a file".to_owned(),
            ));
        }

        Err(new_err(
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "This path does not exist".to_owned(),
        ))
    }

    pub fn import_standard(input: Node) -> Result<Import, Vec<anyhow::Error>> {
        let mut children = input.children();

        let path_node = children.next().unwrap();

        let path = Self::import_path(path_node).to_err_vec()?;

        let (module_type, cached) = input.user_data().import(path.with_extension("ms"))?.clone();

        let no_extension = path.with_extension("");
        let file_name = no_extension.file_name().expect("not a file");

        let ident = Ident::new(
            file_name.to_string_lossy().into_owned(),
            Some(Cow::Owned(TypeLayout::Module(module_type))),
            true,
        );

        // let module_type = file.for_type().details(input.as_span(), &input.user_data().get_source_file_name(), "Could not get the type of this module.").to_err_vec()?;

        input.user_data().add_dependency(&ident);
        // ident.link_force_no_inherit(input.user_data(), Cow::Owned(module_type)).to_err_vec()?;

        // Self::file
        Ok(Import::Standard {
            path,
            store: ident,
            cached,
        })
    }

    pub fn import(input: Node) -> Result<Import, Vec<anyhow::Error>> {
        let unwrapped = input.children().single().to_err_vec()?;

        match unwrapped.as_rule() {
            Rule::import_standard => Self::import_standard(unwrapped),
            x => unreachable!("{x:?}"),
        }
    }
}
