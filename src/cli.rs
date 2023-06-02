use clap::{Parser, Subcommand, ValueEnum};

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

#[derive(ValueEnum, Clone, Debug)]
pub enum CompilationTargets {
    RawText,
    Binary,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Compile and Run a `.ms` file.
    Run {
        /// this is the path to a `.ms` file
        #[arg(required(true), index = 1)]
        path: String,
        /// the stack size, in bytes, allocated to the interpreter. 4 MB default: 4 * 1024 * 1024
        #[arg(short = 'X', long = "stack-size", default_value = "4194304")]
        stack_size: usize,
        /// print extra compilation information, but slows down the process considerably.
        #[arg(long = "verbose", default_value = "false")]
        verbose: bool,
    },
    /// Build the interpreter and have it look for the
    /// entrypoint of a `.mmm` file
    Execute {
        /// this is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
        /// the stack size, in bytes, allocated to the interpreter. 4 MB default: 4 * 1024 * 1024
        #[arg(short = 'X', long = "stack-size", default_value = "4194304")]
        stack_size: usize,
        /// This flag looks for an input path ending in `.transpiled.mmm` and will transpile it before executing.
        #[arg(long = "transpile", default_value = "false")]
        transpile_first: bool,
    },
    /// Compile a `.ms` file into executable bytecode.
    Compile {
        /// this is the path to the file
        #[arg(required(true))]
        path: String,
        /// what should the compiler output: `.transpiled.mmm` human-readable bytecode, or `.mmm` machine code.
        #[arg(value_enum, default_value = "binary", long = "output-format")]
        output_format: CompilationTargets,
        /// print extra compilation information, but slows down the process considerably.
        #[arg(long = "verbose", default_value = "false")]
        verbose: bool,
    },
    /// Transpile human-readable bytecode (`.transpiled.mmm` extension)
    /// into an executable `.mmm` file.
    Transpile {
        /// this is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
    },
}
