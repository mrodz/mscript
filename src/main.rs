#![feature(box_patterns)]
#![allow(dead_code)]

extern crate pest_consume;
extern crate pest_derive;

mod bytecode;
mod cli;

use anyhow::Result;
use bytecode::interpreter::Program;
use clap::Parser;
use cli::Args;
use std::thread;

fn main() -> Result<()> {
    let args = Args::parse();
    // default: 4 MB

    let builder = thread::Builder::new()
        .name("Main".into())
        .stack_size(args.stack_size);

    let handler = builder.spawn(|| -> Result<()> {
        let program = Program::new(args.path)?;

        program.execute()?;

        Ok(())
    })?;

    handler.join().unwrap()?;

    Ok(())
}
