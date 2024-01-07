use std::{
    borrow::Cow,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{Context, Result};
use bytecode::compilation_bridge::id::SPLIT_LOOKUP_STORE;

use crate::{
    ast::CompiledItem,
    instruction,
    parser::{AssocFileData, CompilationLock, Node, Parser, Rule},
    BytecodePathStr, CompilationError, VecErr,
};

use super::{new_err, Compile, Dependencies, Ident, TypeLayout};

#[derive(Debug)]
pub(crate) enum Import {
    Standard {
        path: PathBuf,
        store: Ident,
        should_queue: CompilationLock,
    },
    Names {
        path: PathBuf,
        names: Vec<Ident>,
        should_queue: CompilationLock,
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
                should_queue,
            } => {
                log::debug!("@IMPORT -- will queue: {should_queue}");

                if should_queue.can_compile() {
                    log::debug!("queuing compilation of {path:?}");

                    state.queue_compilation(path.clone());
                    should_queue.mark_compiled();
                }

                let module_loader =
                    format!("{}#__module__", path.with_extension("mmm").bytecode_str());

                Ok(vec![
                    instruction!(module_entry module_loader),
                    instruction!(store(store.name())),
                ])
            }
            Self::Names {
                path,
                names,
                should_queue,
            } => {
                log::debug!("@IMPORT(names) -- will queue: {should_queue}");

                if should_queue.can_compile() {
                    log::debug!("queuing compilation of {path:?}");

                    state.queue_compilation(path.clone());
                    should_queue.mark_compiled();
                }

                let module_loader =
                    format!("{}#__module__", path.with_extension("mmm").bytecode_str());

                let names = names.iter().map(Ident::name).map(String::from).collect();

                Ok(vec![
                    instruction!(module_entry module_loader),
                    CompiledItem::Instruction {
                        id: SPLIT_LOOKUP_STORE,
                        arguments: names,
                    },
                    instruction!(pop),
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

        let path_span = path_node.as_span();

        let path = Self::import_path(path_node).to_err_vec()?;

        let no_extension = path.with_extension("");
        let file_name = no_extension
            .file_name()
            .expect("not a file")
            .to_string_lossy();

        if input.user_data().has_name_been_mapped(&file_name) {
            return Err(vec![new_err(
                path_span,
                &input.user_data().get_source_file_name(),
                "duplicate: this name is already in use".to_owned(),
            )]);
        }

        log::info!("begin @import in standard import");
        let result = input
            .user_data()
            .import(Arc::new(path.with_extension("ms")))?;

        let ident = Ident::new(
            file_name.into_owned(),
            Some(Cow::Owned(TypeLayout::Module(result.module()))),
            true,
        );

        input.user_data().add_dependency(&ident);

        Ok(Import::Standard {
            path,
            store: ident,
            should_queue: CompilationLock::new(),
        })
    }

    pub fn import_names(input: Node) -> Result<Import, Vec<anyhow::Error>> {
        let path_node = input.children().last().unwrap();
        let path = Self::import_path(path_node).to_err_vec()?;

        log::info!("begin @import in import-names");
        let module_import = input
            .user_data()
            .import(Arc::new(path.with_extension("ms")))?;

        let module_type = module_import.module();

        let mut names = vec![];

        for child in input.children() {
            match child.as_rule() {
                Rule::import_type => {
                    let name_node = child.children().next().unwrap();
                    let maybe_ty = name_node.as_str();

                    let ty = module_type
                        .get_type(maybe_ty)
                        .details_lazy_message(
                            child.as_span(),
                            &input.user_data().get_source_file_name(),
                            || {
                                format!(
                                    "`{}` has no visible type `{maybe_ty}`.\n        Please ensure that:\n            - This import is not cyclical (see <https://wikipedia.org/wiki/Circular_reference>)\n            - This item is exported using the `export` keyword\n        Hint: You can put shared code in a new file and import it from both origins to work around a cyclical dependency.",
                                    module_type.name().bytecode_str()
                                )
                            },
                        )
                        .to_err_vec()?;

                    input.user_data().add_type(maybe_ty.into(), ty.clone());
                }
                Rule::import_name => {
                    let maybe_property = child.as_str();

                    let property = module_type
                        .get_property(maybe_property)
                        .details_lazy_message(
                            child.as_span(),
                            &input.user_data().get_source_file_name(),
                            || {
                                format!(
                                    "`{}` has no visible member `{maybe_property}`.\n        Please ensure that:\n            - This item is exported using the `export` keyword\n            - This import is not cyclical (see <https://wikipedia.org/wiki/Circular_reference>)\n        Hint: You can put shared code in a new file and import it from both origins to work around a cyclical dependency.",
                                    module_type.name().bytecode_str()
                                )
                            },
                        )
                        .to_err_vec()?;

                    if input.user_data().has_name_been_mapped(maybe_property) {
                        return Err(vec![new_err(
                            child.as_span(),
                            &input.user_data().get_source_file_name(),
                            "duplicate: this name is already in use".to_owned(),
                        )]);
                    }

                    let ident = property.to_owned();

                    if ident.ty().unwrap().is_class() {
                        input
                            .user_data()
                            .add_type(ident.boxed_name(), ident.ty().cloned().unwrap())
                    }

                    input.user_data().add_dependency(&ident);

                    names.push(ident);
                }
                Rule::import_path => {
                    break;
                }
                unknown => unreachable!("{unknown:?}"),
            }
        }

        Ok(Import::Names {
            path,
            names,
            should_queue: module_import.compilation_lock(),
        })
    }

    pub fn import(input: Node) -> Result<Import, Vec<anyhow::Error>> {
        let unwrapped = input.children().single().to_err_vec()?;

        match unwrapped.as_rule() {
            Rule::import_standard => Self::import_standard(unwrapped),
            Rule::import_names => Self::import_names(unwrapped),
            x => unreachable!("{x:?}"),
        }
    }
}
