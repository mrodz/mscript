// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use anyhow::Result;
use bytecode::{interpreter::{enter_function, functions, open_file}, parse_line};

fn main() -> Result<()> {
    let (path, file) = open_file("src/bytecode/bin/test.mmm").unwrap();

    let functions = functions(file, path).unwrap();

    enter_function(&functions[0]).unwrap();
    enter_function(&functions[1]).unwrap();

    // parse_line(&"constexpr \"Hello, World\"".into());

    // for function in functions {
    //     enter_function(&function).unwrap();
    // }

    Ok(())
}
