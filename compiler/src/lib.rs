mod parser;

use std::{path::Path, fs::File, io::{BufReader, Read}};

use anyhow::Result;

use crate::parser::{Parser, root_node_from_str};

pub fn compile(path: &str) -> Result<()> {
	let file = File::open(path)?;

	let mut reader = BufReader::new(file);

	let mut buffer = String::new();

	reader.read_to_string(&mut buffer)?;

    let input = root_node_from_str(&buffer)?;

	let result: Vec<String> = Parser::file(input)?;

	dbg!(result);

	Ok(())
}