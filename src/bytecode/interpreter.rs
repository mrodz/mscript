use std::fs::File;
use std::io::{BufReader, BufRead};
use std::path::Path;

use anyhow::Result;

#[derive(Parser)]
#[grammar = "bytecode/grammar.pest"]
struct TokenTreeParser;

#[pest_consume::parser]
impl TokenTreeParser {
	
}

struct Attributes {
	
}
struct Function {

}

pub fn interpret<T>(path: T) -> Result<()>
where T: AsRef<Path>
{
	let file = File::open(path)?;
    let mut reader = BufReader::new(file);
	let mut buffer = String::new();

	while let Ok(size) = reader.read_line(&mut buffer) {
		if size == 0 {
			break;
		}
        print!("{}", buffer);
		buffer.clear();
    }

	Ok(())
}
