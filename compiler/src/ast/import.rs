use std::{path::{Path, PathBuf}, borrow::Cow};

use anyhow::{Result, Context};

use crate::{parser::{Parser, Node, Rule, File}, perform_file_io_in, ast_file_from_str, VecErr, instruction, CompilationError, BytecodePathStr};

use super::{Compile, Dependencies, new_err, Ident, IntoType};

#[derive(Debug)]
pub(crate) enum Import {
	Standard {
		path: PathBuf,
		file: File,
		store: Ident,
	}
}

impl Compile for Import {
	fn compile(&self, state: &super::CompilationState) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
		match self {
			Self::Standard { path, file, store } => {

				state.queue_compilation(file.clone());

				let module_loader = format!("{}#__module__", path.with_extension("mmm").bytecode_str());
				Ok(vec![instruction!(call module_loader), instruction!(store (store.name()))])
			}
		}
	}
}

impl Dependencies for Import {}

impl Parser {
	pub fn import_path(input: Node) -> Result<PathBuf> {
		let src: &str = &input.user_data().get_source_file_name();
		let path = Path::new(src);

		let as_str = input.as_str();
		// let mut children = input.children();
		let attempted_path = Path::new(as_str);

		let path = path.parent().context("no parent")?.join(attempted_path);

		let path_exists = path.try_exists()?;

		let with_extension = path.with_extension("ms");
		
		if with_extension.try_exists()? {
			return Ok(with_extension.to_path_buf())
		}

		if path_exists && path.is_dir() {
			return Err(new_err(input.as_span(), src, "This is a directory, and not a file".to_owned()));
		}
		
		Err(new_err(input.as_span(), src, "This path does not exist".to_owned()))
	}

	pub fn import_standard(input: Node) -> Result<Import, Vec<anyhow::Error>> {
		let mut children = input.children();

		let path_node = children.next().unwrap();

		let path = Self::import_path(path_node).to_err_vec()?; //.canonicalize().context("path not found").to_err_vec()?;

		let in_buffer = perform_file_io_in(&path).to_err_vec()?;
		let file = ast_file_from_str(&path, &path.with_extension("mmm"), &in_buffer)?;

		let file_name = path.file_name().expect("not a file");

		let mut ident = Ident::new(file_name.to_string_lossy().into_owned(), None, true);

		let module_type = file.for_type().details(input.as_span(), &input.user_data().get_source_file_name(), "Could not get the type of this module.").to_err_vec()?;

		ident.link_force_no_inherit(input.user_data(), Cow::Owned(module_type)).to_err_vec()?;

		// Self::file
		Ok(Import::Standard { path, file, store: ident })
	}

	pub fn import(input: Node) -> Result<Import, Vec<anyhow::Error>> {
		let unwrapped = input.children().single().to_err_vec()?;

		match unwrapped.as_rule() {
			Rule::import_standard => {
				Self::import_standard(unwrapped)
			}
			x => unreachable!("{x:?}")
		}
	}
}