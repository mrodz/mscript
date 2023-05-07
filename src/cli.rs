use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Main CLI for MScript",
    long_about = "This is the CLI toolchain for MScript. You can run bytecode, transpile bytecode, and compile bytecode."
)]
pub struct Args {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Build the interpreter and have it look for the
    /// entrypoint of a `.mmm` file
    Run {
        /// this is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
        /// 4 MB default: 4 * 1024 * 1024
        #[arg(short = 'X', long = "stack-size", default_value = "4194304")]
        stack_size: usize,
    },
    /// Compile a `.ms` file into executable bytecode.
    Compile {
        /// this is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
    },
    /// Transpile human-readable bytecode (`.transpiled.mmm` extension)
    /// into an executable `.mmm` file.
    Transpile {
        /// this is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
    },
}
