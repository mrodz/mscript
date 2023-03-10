// #![feature(let_else)] // for backwards compatibility

#[macro_use]
extern crate pest_derive;
extern crate pest_consume;

mod bytecode;

use anyhow::Result;

fn main() -> Result<()> {
    bytecode::interpreter::interpret("src/bytecode/bin/test.mmm")?;

    Ok(())
}
