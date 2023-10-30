use std::{fmt::Display, path::{Path, PathBuf}};

use anyhow::{Result, Context};

use crate::{parser::{Parser, Node, Rule, File}, perform_file_io_in, ast_file_from_str, VecErr, compile};

use super::{Compile, Dependencies, new_err};

#[derive(Debug)]
pub(crate) enum ImportPathFeature {
	RelativeSelf,
	RelativeParent,
	Directory(Box<str>),
	File(Box<str>),
}

impl Display for ImportPathFeature {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			Self::RelativeSelf => ".",
			Self::RelativeParent => "..",
			Self::Directory(name) | Self::File(name)=> name,
		})
	}
}

#[derive(Debug)]
pub(crate) struct ImportPath {
	this: ImportPathFeature,
	next: Option<Box<ImportPath>>,
}

const PATH_SEP: &str = "/";

impl Display for ImportPath {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.this)?;

		if let Some(next) = &self.next {
			write!(f, "{PATH_SEP}{next}")?;
		}

		Ok(())
	}
}

#[derive(Debug)]
pub(crate) enum Import {
	Standard {
		path: PathBuf,
		file: File,
	}
}

impl Compile for Import {
	fn compile(&self, state: &super::CompilationState) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
		match self {
			Self::Standard { path, file } => {
				todo!()
				// compile(, output_bin, verbose, output_to_file)

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

		dbg!(&path);

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

		let path = Self::import_path(path_node).to_err_vec()?;

		let in_buffer = perform_file_io_in(&path).to_err_vec()?;
		let file = ast_file_from_str(&path, &path.with_extension("mmm"), &in_buffer)?;

		// Self::file
		Ok(Import::Standard { path, file })
	}

	pub fn import(input: Node) -> Result<Import, Vec<anyhow::Error>> {
		let unwrapped = input.children().single().to_err_vec()?;

		match unwrapped.as_rule() {
			Rule::import_standard => {
				return Self::import_standard(unwrapped)
			}
			x => unreachable!("{x:?}")
		}
	}
}