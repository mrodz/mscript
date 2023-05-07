#![feature(box_patterns)]

mod cli;

use anyhow::{Result, Context};
use bytecode::Program;
use bytecode_dev_transpiler;
use clap::Parser;
use cli::{Args, Commands};
use std::thread;

fn main() -> Result<()> {
    let args = Args::parse();

    let command = args.command;

    match command {
        Commands::Run { path, stack_size } => {
            let builder = thread::Builder::new()
                .name("Main".into())
                .stack_size(stack_size);

            let handler = builder.spawn(|| -> Result<()> {
                let program = Program::new(path)?;

                program.execute()?;

                Ok(())
            })?;
            handler.join().unwrap()?;
        }
        Commands::Transpile { path } => {
            bytecode_dev_transpiler::transpile_file(path).context("Could not transpile file")?;
        }
        Commands::Compile { path: _ } => {
            unimplemented!();
        }
    }

    Ok(())
}
