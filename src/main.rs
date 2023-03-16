// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use anyhow::Result;
use bytecode::interpreter::Program;

fn main() -> Result<()> {
    let _ = Program::new("src/bytecode/bin/test.mmm")?.execute()?;

    Ok(())
}
