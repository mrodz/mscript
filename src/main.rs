// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use anyhow::{Context, Result};
use bytecode::interpreter::{functions, open_file};

fn main() -> Result<()> {
    let (path, file) = open_file("src/bytecode/bin/test.mmm")?;

    let functions = functions(file, path)?;

    functions["src/bytecode/bin/test.mmm#do_math"]
        .run()
        .context("function exited abruptly")?;
    functions["src/bytecode/bin/test.mmm#floating_point_math"]
        .run()
        .context("function exited abruptly")?;

    Ok(())
}
