// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use anyhow::Result;
use bytecode::interpreter::{open_file, functions, enter_function};

fn main() -> Result<()> {
    let (path, file) = open_file("src/bytecode/bin/test.mmm").unwrap();

    let functions = functions(file, path).unwrap();

    enter_function(&functions[0]).unwrap();
    // for function in functions {
    //     enter_function(&function).unwrap();
    // }

    Ok(())
}
