use anyhow::Result;

#[macro_use]
extern crate pest_derive;
extern crate pest_consume;

mod bytecode;

fn main() -> Result<()> {
    bytecode::interpreter::interpret("src/bytecode/bin/test.mmm")?;

    Ok(())
}
