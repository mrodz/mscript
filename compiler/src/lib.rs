#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(iter_collect_into)]
#![feature(try_blocks)]

mod ast;
mod parser;
mod scope;

use std::borrow::Cow;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;
use std::thread;
use std::time::{Duration, Instant};

use anyhow::{bail, Result};
use ast::Compile;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle, ProgressFinish};
use once_cell::sync::Lazy;
use parser::AssocFileData;
use scope::ScopeType;

use crate::ast::CompiledItem;
use crate::parser::{root_node_from_str, Parser};

pub fn compile(path_str: &str, output_bin: bool) -> Result<()> {
    let start_time = Instant::now();
    let path = Path::new(path_str);

    let Some(ext) = path.extension() else {
        bail!("no file extension")
    };

    if !ext.eq_ignore_ascii_case("ms") {
        bail!("MScript uses `.ms` file extensions. Please check your file extensions.")
    }

    let output_path = path.with_extension("mmm");

    let file = File::open(path)?;

    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    reader.read_to_string(&mut buffer)?;

    let files = MultiProgress::new();
    // pb.set_style(ProgressStyle::with_template("{prefix:.bold.dim} [{elapsed_precise}] {wide_bar:.cyan/blue}")?);

    static SPINNER_STYLE: Lazy<ProgressStyle> = Lazy::new(|| {
        ProgressStyle::with_template(
            "[{elapsed_precise:.bold.green}] {prefix:.bold.dim} {spinner} {wide_msg:.yellow}",
        )
        .unwrap()
        .tick_chars("|/-\\ ")
    });

    fn make_spinner(group: &MultiProgress, message: impl Into<Cow<'static, str>>) -> ProgressBar {
        let spinner = group.add(ProgressBar::new_spinner().with_finish(ProgressFinish::AbandonWithMessage("!!! Error !!!".into())));

        spinner.set_prefix(message);
        spinner.set_style(SPINNER_STYLE.clone());
        spinner.enable_steady_tick(Duration::from_millis(50));

        spinner
    }

    let user_data = AssocFileData::new(ScopeType::File, output_path.to_string_lossy().to_string());

    let parse_spinner = make_spinner(&files, format!("Parsing ({path_str}):"));
    let input = root_node_from_str(&buffer, user_data)?;
    parse_spinner.finish_with_message(format!(
        "Done in {} ms",
        parse_spinner.elapsed().as_millis()
    ));

    let ast_spinner = make_spinner(&files, format!("Creating AST ({path_str}):"));
    let result = Parser::file(input)?;
    ast_spinner.finish_with_message(format!("Done in {} ms", ast_spinner.elapsed().as_millis()));

    let compilation_spinner = make_spinner(&files, format!("Compiling AST ({path_str}):"));

    let mut function_buffer = vec![];
    result.compile(&mut function_buffer)?;
    compilation_spinner.finish_with_message(format!(
        "Done in {} ms",
        compilation_spinner.elapsed().as_millis()
    ));

    // function_buffer is initialized now.

    let optimizing_spinner = make_spinner(&files, format!("Optimizing ({path_str}):"));

    optimizing_spinner.finish_with_message(format!(
        "Done in {} ms",
        optimizing_spinner.elapsed().as_millis()
    ));

    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path)?;

    let writing_pb =
        files.add(ProgressBar::new(function_buffer.len() as u64).with_prefix("Writing to file"));
    writing_pb.set_style(
        ProgressStyle::with_template(
            "[{elapsed_precise:.bold.green}] {prefix:.bold.dim} [{bar:40.blue/red}] {msg:.yellow}",
        )?
        .progress_chars("=> "),
    );

    for function in writing_pb.wrap_iter(function_buffer.iter()) {
        let this_repr = function.repr(!output_bin);

        let bytes = this_repr.as_bytes();

        if let CompiledItem::Function { id, .. } = function {
            writing_pb.set_message(format!("f {} ({} bytes)", id.to_string(), bytes.len()));
        }

        new_file.write_all(bytes)?;
    }

    writing_pb.finish_with_message(format!("Done in {} ms", writing_pb.elapsed().as_millis()));
    println!("Compiled in {} ms", start_time.elapsed().as_millis());

    Ok(())
}
