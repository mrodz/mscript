use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
	/// this is the path to the file
    #[arg(required(true), index = 1)]
    pub path: String,
	/// 4 MB default: 4 * 1024 * 1024
	#[arg(short = 'X', long = "stack-size", default_value = "4194304")]
	pub stack_size: usize,
	/// turn bytecode into text representation
	#[arg(long = "bin-to-repr")]
	pub into_repr: bool,
	#[arg(long = "repr-to-bin")]
	pub into_bin: bool,
}