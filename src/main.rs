// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use std::process::exit;

use anyhow::{bail, Context, Result};
use bytecode::interpreter::{functions, open_file};

use crate::bytecode::Stack;

fn main() -> Result<()> {
    let (path, file) = open_file("src/bytecode/bin/test.mmm")?;

    let mut functions = functions(file, path)?;
    let mut stack = Stack::new();
    stack.extend(&"__global__#interpreter".into());

    let res = functions["src/bytecode/bin/test.mmm#floating_point_math"]
        .run(&mut stack)
        .with_context(|| format!("\r\n{stack}"))?;

    println!("Program exited normally (returned {res})");

    let res = functions["src/bytecode/bin/test.mmm#divide_by_zero"]
        .run(&mut stack)
        .with_context(|| format!("\r\n{stack}"))?;

    Ok(())
}
