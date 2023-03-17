// #![feature(let_else)] // for backwards compatibility

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;

use std::thread;

use anyhow::Result;
use bytecode::interpreter::Program;

fn main() -> Result<()> {
    // default: 4 MB
    let stack_size = 4 * 1024 * 1024;

    let builder = thread::Builder::new().name("interpreter".into()).stack_size(stack_size); 
    
    let handler = builder.spawn(|| {
        let program = Program::new("src/bytecode/bin/test.mmm").expect("Could not build program");
        program.execute().expect("failure!");
    })?;

    handler.join().unwrap();
    
    Ok(())
}