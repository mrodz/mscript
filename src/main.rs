// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use anyhow::Result;
use bytecode::MScriptFile;

use crate::bytecode::Stack;

fn main() -> Result<()> {

    let mut file = MScriptFile::open("src/bytecode/bin/test.mmm")?;

    let mut stack = Stack::new();

    file.run_function("floating_point_math", &mut stack)?;
    file.run_function("divide_by_zero", &mut stack)?;

    Ok(())
}
