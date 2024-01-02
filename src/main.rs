mod cli;

use anyhow::{bail, Context, Result};
use bytecode::{compilation_bridge::MScriptFile, Program};
use clap::Parser;
use cli::{Args, Commands};
use colored::*;
use compiler::compile as compile_file;
use log::{Level, LevelFilter, Metadata, Record};
use std::{
    fmt::{Debug, Display},
    io::{stdout, Write},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
    thread,
    time::Instant,
};

use crate::cli::CompilationTargets;

fn compile(
    path_str: &str,
    output_bin: bool,
    verbose: bool,
    output_to_file: bool,
    override_no_pb: bool,
) -> Result<Option<Rc<MScriptFile>>> {
    let compilation = compile_file(
        path_str,
        output_bin,
        verbose,
        output_to_file,
        override_no_pb,
    );
    match compilation {
        Err(errors) => {
            let cerr = errors.len();
            for error in &errors {
                println!("{error:?}")
            }

            let plural_char = if cerr > 1 { "s" } else { "" };

            bail!("Did not compile successfully ({cerr} Error{plural_char})")
        }
        Ok(product) => Ok(product),
    }
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
                Level::Error => "[ Error ]".bright_white().on_red(),
                Level::Warn => "[ Warning ]".black().on_yellow(),
                Level::Info => "[ Info ]".white().on_bright_blue(),
                Level::Trace => "[ Trace ]".on_green(),
            }
        }

        if self.enabled(record.metadata()) {
            println!("{} {}", level_format(&record.level()), record.args());
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
            quick,
            override_no_pb,
            profile,
        } => {
            if verbose && !quick {
                LOGGER.set_verbose();
            }

            if let Err(err) =
                log::set_logger(&LOGGER).map(|()| log::set_max_level(LevelFilter::Trace))
            {
                bail!("Error initializing logger: {err:?}")
            };

            let builder = thread::Builder::new()
                .name("mscript-runtime".into())
                .stack_size(stack_size);

            let main_thread = builder.spawn(move || -> Result<(Result<()>, Option<Instant>)> {
                let Some(product) = compile(&path, true, !quick, false, override_no_pb)? else {
                    unreachable!();
                };

                if verbose {
                    println!("Running...\n");
                }

                stdout().flush()?;

                let start = if profile { Some(Instant::now()) } else { None };

                let program = Program::new_from_file(product);

                Ok((program.execute(), start))
            })?;

            let finished = main_thread.join();

            if profile {
                let secs = if let Ok(Ok((_, Some(start)))) = finished {
                    let duration = Instant::now().duration_since(start);
                    duration.as_secs_f32()
                } else {
                    -1.0
                };

                println!(
                    "{}",
                    format!("\nRuntime Profile:\n  Program terminated in {secs}s",).dimmed()
                );

                if let Some(profile) = memory_stats::memory_stats() {
                    println!(
                        "{}",
                        format!(
                            "  Memory Usage:\n    Physical memory: {} bytes\n    Virtual memory: {} bytes",
                            profile.physical_mem, profile.virtual_mem
                        )
                        .dimmed()
                    );
                } else {
                    println!("{}", "    <system does not support memory stats>".dimmed())
                }

                let thread_ok = finished.is_ok();
                let (job_ok, job_view) = if let Ok(ref finished) = finished {
                    (finished.is_ok(), Some(finished))
                } else {
                    (false, None)
                };
                let execution_ok = matches!(job_view, Some(Ok((Ok(_), _))));

                println!(
                    "{}",
                    format!(
                        "  Exit:\n    Runner thread status: {}\n    Task status: {}\n    Program exit: {}\n",
                        if thread_ok { "ok" } else { "failed"},
                        if job_ok { "ok" } else { "failed" },
                        if execution_ok { "ok" } else { "failed" },
                    )
                    .dimmed()
                );
            }

            if let Err(e) = finished {
                trait ObjectSafeErrorKind: Send + Sync + Debug + Display {}
                match e.downcast::<Box<dyn ObjectSafeErrorKind>>() {
                    Ok(downcast) => {
                        log::debug!("Runtime dynamic cast `Any` error");
                        bail!(downcast);
                    }
                    Err(e) => std::panic::resume_unwind(e),
                }
            } else {
                let (program_exit_status, _) = finished.unwrap()?;
                program_exit_status?;
            }
        }
        Commands::Execute {
            path,
            stack_size,
            transpile_first,
        } => {
            let builder = thread::Builder::new()
                .name("mscript-runtime".into())
                .stack_size(stack_size);

            let main_thread = builder.spawn(move || -> Result<()> {
                let program = if transpile_first {
                    println!("=======================\n");
                    let new_path = transpile_command(&path)?;
                    println!("\nRunning the outputted file...\n=======================\n");
                    Program::new(new_path)?
                } else {
                    Program::new(path)?
                };

                program.execute()
            })?;

            main_thread.join().unwrap()?;
        }
        Commands::Transpile { path } => {
            transpile_command(&path)?;
        }
        Commands::Compile {
            path,
            output_format,
            verbose,
            quick,
        } => {
            let should_log = verbose && !quick;
            if should_log {
                LOGGER.set_verbose();
            }

            if let Err(err) =
                log::set_logger(&LOGGER).map(|()| log::set_max_level(LevelFilter::Trace))
            {
                bail!("Error initializing logger: {err:?}")
            };

            let output_bin = matches!(output_format, CompilationTargets::Binary);
            compile(&path, output_bin, !quick, true, false)?;
        }
    }

    Ok(())
}
