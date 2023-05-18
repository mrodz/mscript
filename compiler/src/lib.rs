#![feature(iter_intersperse)]
#![feature(iter_collect_into)]

mod parser;
mod ast;

use std::fs::File;
use std::io::{BufReader, Read};

use anyhow::Result;
use ast::Compile;

use crate::parser::{Parser, root_node_from_str};

pub fn compile(path: &str) -> Result<()> {
	let file = File::open(path)?;

	let mut reader = BufReader::new(file);

	let mut buffer = String::new();

	reader.read_to_string(&mut buffer)?;

    let input = root_node_from_str(&buffer)?;

	let result = Parser::file(input)?;

	let compiled = result.compile();

	// dbg!(result);

	Ok(())
}