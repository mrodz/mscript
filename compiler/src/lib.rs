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
use std::rc::Rc;
use std::time::{Duration, Instant};

use anyhow::{bail, Result, Context};
use ast::Compile;
use indicatif::{MultiProgress, ProgressBar, ProgressFinish, ProgressStyle};

use once_cell::sync::Lazy;
use parser::AssocFileData;
use scope::ScopeType;

use crate::ast::CompiledItem;
use crate::parser::{root_node_from_str, Parser};

struct VerboseLogger {
    progress_bars: Option<MultiProgress>,
}

static SPINNER_STYLE: Lazy<ProgressStyle> = Lazy::new(|| {
    ProgressStyle::with_template(
        "[{elapsed_precise:.bold.green}] {prefix:.bold.dim} {spinner} {wide_msg:.yellow}",
    )
    .unwrap()
    .tick_chars("|/-\\ ")
});

impl VerboseLogger {
    fn new(is_active: bool) -> Self {
        let progress_bars = if is_active {
            Some(MultiProgress::new())
        } else {
            None
        };

        Self { progress_bars }
    }

    fn add(&self, supplier: impl FnOnce() -> ProgressBar) -> Option<ProgressBar> {
        if let Some(ref progress_bars) = self.progress_bars {
            let pb = supplier();
            return Some(progress_bars.add(pb));
        }

        None
    }

    fn wrap_in_spinner<R>(
        &self,
        message: impl Into<Cow<'static, str>>,
        action: impl FnOnce() -> Result<R>,
    ) -> Result<R> {
        let Some(ref progress_bars) = self.progress_bars else {
            return action();
        };

        let spinner = progress_bars.add(
            ProgressBar::new_spinner()
                .with_finish(ProgressFinish::AbandonWithMessage("!!! Error !!!".into()))
                .with_prefix(message)
                .with_style(SPINNER_STYLE.clone()),
        );

        spinner.enable_steady_tick(Duration::from_millis(50));

        let result = action()?;

        spinner.finish_with_message(format!("Done in {:?}", spinner.elapsed()));

        Ok(result)
    }
}

pub trait Maybe<T> {
    type Output;
    fn maybe(&self, action: impl FnOnce(&T)) -> &Self::Output;
}

impl<T> Maybe<T> for Option<T> {
    type Output = Option<T>;
    fn maybe(&self, action: impl FnOnce(&T)) -> &Self::Output {
        if let Some(x) = self {
            action(x);
        }
        self
    }
}

pub fn compile(path_str: &str, output_bin: bool, verbose: bool) -> Result<()> {
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

    let logger = VerboseLogger::new(verbose);

    let user_data = Rc::new(AssocFileData::new(
        ScopeType::File,
        output_path.to_string_lossy().to_string(),
    ));

    let input = logger.wrap_in_spinner(format!("Parsing ({path_str}):"), || {
        root_node_from_str(&buffer, user_data)
    })?;

    let file = logger.wrap_in_spinner(format!("Creating AST ({path_str}):"), || {
        Parser::file(input)
    })?;

    let mut function_buffer = vec![];
    logger.wrap_in_spinner(format!("Compiling AST ({path_str}):"), || {
        file.compile(&mut function_buffer)
    })?;
    // function_buffer is initialized now.

    // TODO:
    let _optimized = logger.wrap_in_spinner(format!("Optimizing ({path_str}):"), || Ok(()))?;

    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path)?;

    let writing_pb = logger.add(|| {
        let template =
            "[{elapsed_precise:.bold.green}] {prefix:.bold.dim} [{bar:40.blue/red}] {msg:.yellow}";
        let style = ProgressStyle::with_template(template)
            .unwrap()
            .progress_chars("=> ");

        ProgressBar::new(function_buffer.len() as u64)
            .with_prefix("Writing to file")
            .with_style(style)
    });

    let iter = function_buffer.iter();

    let mut for_each = |function: &CompiledItem| -> Result<()> {
        let this_repr = function.repr(!output_bin).context("getting the string representation of a compiled item")?;

        let bytes = this_repr.as_bytes();

        if let CompiledItem::Function { id, .. } = function {
            writing_pb.maybe(|writing_pb| {
                writing_pb.set_message(format!("f {} ({} bytes)", id.to_string(), bytes.len()))
            });
        }

        new_file.write_all(bytes)?;

        Ok(())
    };

    if let Some(ref writing_pb) = writing_pb {
        for x in writing_pb.wrap_iter(iter) {
            for_each(x).with_context(|| format!("{function_buffer:#?}"))?;
        }
    } else {
        for x in iter {
            for_each(x).with_context(|| format!("{function_buffer:#?}"))?;
        }
    }

    writing_pb.maybe(|writing_pb| writing_pb.finish_with_message(format!("Done in {} ms", writing_pb.elapsed().as_millis())));
    println!("Compiled in {:?}", start_time.elapsed());

    Ok(())
}
