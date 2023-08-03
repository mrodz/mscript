mod cli;

use anyhow::{bail, Context, Result};
use colored::*;
use bytecode::Program;
use clap::Parser;
use cli::{Args, Commands};
use compiler::compile as compile_file;
use log::{Level, LevelFilter, Metadata, Record};
use std::{
    path::{Path, PathBuf},
    thread, sync::Mutex,
};

use crate::cli::CompilationTargets;

fn compile(path_str: &str, output_bin: bool, verbose: bool) -> Result<()> {
    if let Err(errors) = compile_file(path_str, output_bin, verbose) {
        let cerr = errors.len();
        for error in &errors {
            println!("{error:?}")
        }

        let plural_char = if cerr > 1 { "s" } else { "" };

        bail!("Did not compile successfully ({cerr} Error{plural_char})")
    }

    Ok(())
}

fn transpile_command(path: &String) -> Result<Box<str>> {
    if !bytecode_dev_transpiler::is_path_a_transpiled_source(path) {
        bail!("The standard extension for file transpilation sources is `.mmm.transpiled`. Please check your file extensions. (Found {path})")
    }

    let new_path = Path::new(&path).with_extension("").with_extension("mmm");

    let Some(new_path) = new_path.to_str() else {
        bail!("path is not valid unicode")
    };

    bytecode_dev_transpiler::transpile_file(path, new_path).context("Could not transpile file")?;

    Ok(new_path.into())
}

fn execute_command(path: String, stack_size: usize, transpile_first: bool) -> Result<()> {
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

    handler.join().unwrap()
}

pub fn is_path_source(input: &String) -> Result<PathBuf> {
    let path = Path::new(input);
    if let Some(ext) = path.extension() {
        if ext.eq_ignore_ascii_case("ms") {
            return Ok(path.with_extension("mmm"));
        }
    }
    bail!("Please use the .ms file extension for MScript source files.")
}

struct GlobalLogger(Mutex<bool>);

impl GlobalLogger {
    const fn new() -> Self {
        Self(Mutex::new(false))
    }

    fn set_verbose(&self) {
        let mut lock = self.0.lock().unwrap();
        *lock = true;
    }

    pub fn is_verbose(&self) -> bool {
        let lock = self.0.lock().unwrap();
        *lock
    } 

}

impl log::Log for GlobalLogger {
    fn enabled(&self, _: &Metadata) -> bool {
        self.is_verbose()
    }

    fn log(&self, record: &Record) {
        fn level_format(level: &Level) -> ColoredString {
            match level {
                Level::Debug => "[ Debug ]".white().on_purple(),
                Level::Error => "[ Error ]".bright_red().on_white(),
                Level::Warn => "[ Warning ]".black().on_yellow(),
                Level::Info => "[ Info ]".white().on_bright_blue(),
                Level::Trace => "[ Trace ]".on_green(),
            }
        }
    
        if self.enabled(record.metadata()) {
            println!(
                "{} {}",
                level_format(&record.level()),
                record.args()
            );
        }
    }

    fn flush(&self) {}
}

static LOGGER: GlobalLogger = GlobalLogger::new();

fn main() -> Result<()> {
    let args = Args::parse();

    let command = args.command;

    match command {
        Commands::Run {
            path,
            stack_size,
            verbose,
            quick
        } => {
            if verbose && !quick {
                LOGGER.set_verbose();
            }

            if let Err(err) = log::set_logger(&LOGGER).map(|()| log::set_max_level(LevelFilter::Trace)) {
                bail!("Error initializing logger: {err:?}")
            };
        
            let output_path = is_path_source(&path)?;
            compile(&path, true, !quick)?;
            println!("Running...\n");
            execute_command(output_path.to_string_lossy().to_string(), stack_size, false)?
        }
        Commands::Execute {
            path,
            stack_size,
            transpile_first,
        } => execute_command(path, stack_size, transpile_first)?,
        Commands::Transpile { path } => {
            transpile_command(&path)?;
        }
        Commands::Compile {
            path,
            output_format,
            verbose,
            quick,
        } => {
            if verbose && !quick {
                LOGGER.set_verbose();
            }

            if let Err(err) = log::set_logger(&LOGGER).map(|()| log::set_max_level(LevelFilter::Trace)) {
                bail!("Error initializing logger: {err:?}")
            };

            let output_bin = matches!(output_format, CompilationTargets::Binary);
            compile(&path, output_bin, !quick)?;
        }
    }

    Ok(())
}
