#![feature(iter_intersperse)]
#![feature(iter_collect_into)]

mod ast;
mod parser;
mod scope;

use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;

use anyhow::{bail, Result};
use ast::Compile;
use parser::AssocFileData;
use scope::ScopeType;

use crate::parser::{root_node_from_str, Parser};

pub fn compile(path: &str, output_bin: bool) -> Result<()> {
    let path = Path::new(path);

    let Some(ext) = path.extension() else {
        bail!("no file extension")
    };

    if !ext.eq_ignore_ascii_case("ms") {
        bail!("MScript uses `.ms` file extensions. Please check your file extensions.")
    }

    let output_path = path.with_extension("mmm");

    let file = File::open(path)?;

    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    reader.read_to_string(&mut buffer)?;

    let user_data = AssocFileData::new(ScopeType::File, output_path.to_string_lossy().to_string());

    let input = root_node_from_str(&buffer, user_data)?;

    let result = Parser::file(input)?;

    let compiled_functions = result.compile()?;

    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path)?;

    for function in compiled_functions {
        let this_repr = function.repr(!output_bin);
        new_file.write_all(this_repr.as_bytes())?;
    }

    Ok(())
}
