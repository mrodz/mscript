#![feature(box_patterns)]

mod cli;

use anyhow::{bail, Context, Result};
use bytecode::Program;
use bytecode_dev_transpiler;
use clap::Parser;
use cli::{Args, Commands};
use compiler::compile;
use std::{path::Path, thread};

fn main() -> Result<()> {
    let args = Args::parse();

    let command = args.command;

    fn transpile_command(path: &String) -> Result<Box<str>> {
        if !bytecode_dev_transpiler::is_path_a_transpiled_source(&path) {
            bail!("The standard extension for file transpilation sources is `.mmm.transpiled`. Please check your file extensions. (Found {path})")
        }

        let new_path = Path::new(&path)
            .with_extension("")
            .with_extension("mmm");

        let Some(new_path) = new_path.to_str() else {
            bail!("path is not valid unicode")
        };

        bytecode_dev_transpiler::transpile_file(&path, new_path)
            .context("Could not transpile file")?;

        Ok(new_path.into())
    }

    match command {
        Commands::Run {
            path,
            stack_size,
            transpile_first,
        } => {
            let builder = thread::Builder::new()
                .name("Main".into())
                .stack_size(stack_size);

            let handler = builder.spawn(move || -> Result<()> {
                let program = if transpile_first {
                    println!("=======================\n");
                    let new_path = transpile_command(&path)?;
                    println!("\nRunning the outputted file...\n=======================\n");
                    Program::new(new_path)?
                } else {
                    Program::new(path)?
                };

                program.execute()?;

                Ok(())
            })?;


            handler.join().unwrap()?;
        }
        Commands::Transpile { path } => {
            transpile_command(&path)?;
        }
        Commands::Compile { path } => {
            compile(&path)?;
        }
    }

    Ok(())
}
