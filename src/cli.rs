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
        /// This is the path to a `.ms` file
        #[arg(required(true), index = 1)]
        path: String,
        /// The stack size, in bytes, allocated to the runtime. 4 MB default: 4 * 1024 * 1024
        #[arg(short = 'X', long = "stack-size", default_value = "4194304")]
        stack_size: usize,
        /// Print extra compilation information, but slows down the process considerably.
        #[arg(long = "verbose", default_value = "false")]
        verbose: bool,
        /// No debug or display info besides the minimum.
        #[arg(short = 'q', long = "quick", default_value = "false")]
        quick: bool,
        /// Override any display settings related to the progress bars and disable them.
        #[arg(long = "no-pb", default_value = "false")]
        override_no_pb: bool,
        /// Profile the runtime and memory usage of a program.
        #[arg(long = "profile", default_value = "false")]
        profile: bool,
    },
    /// Spawn an interpreter and have it execute the module
    /// entrypoint of a compiled MScript `.mmm` file
    Execute {
        /// This is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
        /// The stack size, in bytes, allocated to the interpreter. 4 MB default: 4 * 1024 * 1024
        #[arg(short = 'X', long = "stack-size", default_value = "4194304")]
        stack_size: usize,
        /// This flag looks for an input path ending in `.transpiled.mmm` and will transpile it before executing.
        #[arg(long = "transpile", default_value = "false")]
        transpile_first: bool,
    },
    /// Compile a `.ms` file into executable bytecode.
    Compile {
        /// This is the path to the file
        #[arg(required(true))]
        path: String,
        /// What should the compiler output: `.transpiled.mmm` human-readable bytecode, or `.mmm` machine code.
        #[arg(value_enum, default_value = "binary", long = "output-format")]
        output_format: CompilationTargets,
        /// Print extra compilation information, but slows down the process considerably.
        #[arg(long = "verbose", default_value = "false")]
        verbose: bool,
        /// No debug or display info besides the minimum.
        #[arg(long = "quick", default_value = "false")]
        quick: bool,
    },
    /// Transpile human-readable bytecode (`.transpiled.mmm` extension)
    /// into an executable `.mmm` file.
    Transpile {
        /// This is the path to the file
        #[arg(required(true), index = 1)]
        path: String,
    },
    /// Remove MScript's temporary files and directories.
    Clean {
        #[arg(default_value = ".")]
        path: String
    }
}
