/// for #[derive()] macro in `parser.rs`
extern crate alloc;

mod ast;
mod parser;
mod scope;

use std::borrow::Cow;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;
use std::rc::Rc;
use std::time::{Duration, Instant};

use anyhow::{anyhow, Context, Result};
use ast::Compile;
use indicatif::{MultiProgress, ProgressBar, ProgressFinish, ProgressStyle};

use once_cell::sync::Lazy;
use parser::AssocFileData;

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
        action: impl FnOnce() -> Result<R, Vec<anyhow::Error>>,
    ) -> Result<R, Vec<anyhow::Error>>
    {
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

pub trait VecErr<T> {
    fn to_err_vec(self) -> Result<T, Vec<anyhow::Error>>;
}

impl <T>VecErr<T> for Result<T> {
    fn to_err_vec(self) -> Result<T, Vec<anyhow::Error>> {
        self.map_err(|e| vec![e])
    }
}

/// # Todo
/// Unit tests can use this function
#[allow(unused)]
pub(crate) fn compile_str(mscript_code: &str, unit_name: &str) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
    let logger = VerboseLogger::new(false);

    let input_path = format!("{unit_name}.ms");
    let output_path = format!("{unit_name}.mmm");

    compile_from_str(&logger, Path::new(&input_path), Path::new(&output_path), mscript_code)
}

pub(crate) fn compile_from_str(
    logger: &VerboseLogger,
    input_path: &Path,
    output_path: &Path,
    mscript_code: &str,
) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
    let user_data = Rc::new(AssocFileData::new(
        output_path.to_string_lossy().to_string(),
    ));

    let input = logger.wrap_in_spinner(format!("Parsing ({input_path:?}):"), || {
        root_node_from_str(mscript_code, user_data.clone())
    })?;

    let file =
        logger.wrap_in_spinner(format!("Creating AST ({input_path:?}):"), || Parser::file(input))?;

    let mut function_buffer = vec![];
    logger.wrap_in_spinner(format!("Validating AST ({input_path:?}):"), || {
        file.compile(&mut function_buffer)?;

        Ok(())
    })?;

    Ok(function_buffer)
}

pub fn compile(path_str: &str, output_bin: bool, verbose: bool) -> Result<(), Vec<anyhow::Error>> {
    let start_time = Instant::now();
    
    let input_path = Path::new(path_str);

    let Some(ext) = input_path.extension() else {
        return Err(anyhow!("no file extension")).to_err_vec();
    };

    if !ext.eq_ignore_ascii_case("ms") {
        return Err(anyhow!("MScript uses `.ms` file extensions. Please check your file extensions.")).to_err_vec();
    }

    let output_path = input_path.with_extension("mmm");

    let file = File::open(input_path).map_err(|e| vec![anyhow!(e)])?;

    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    reader.read_to_string(&mut buffer).map_err(|e| vec![anyhow!(e)])?;

    let logger = VerboseLogger::new(verbose);

    let function_buffer = compile_from_str(&logger, input_path, &output_path, &buffer)?;

    logger.wrap_in_spinner(format!("Optimizing ({path_str}):"), || Ok(()))?;

    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path).map_err(|e| vec![anyhow!(e)])?;

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
        let this_repr = function
            .repr(!output_bin)
            .context("getting the string representation of a compiled item")?;

        let bytes = this_repr.as_bytes();

        if let CompiledItem::Function { id, .. } = function {
            writing_pb.maybe(|writing_pb| {
                writing_pb.set_message(format!("f {} ({} bytes)", id, bytes.len()))
            });
        }

        new_file.write_all(bytes)?;

        Ok(())
    };

    if let Some(ref writing_pb) = writing_pb {
        for x in writing_pb.wrap_iter(iter) {
            for_each(x).with_context(|| format!("{function_buffer:#?}")).to_err_vec()?;
        }
    } else {
        for x in iter {
            for_each(x).with_context(|| format!("{function_buffer:#?}")).to_err_vec()?;
        }
    }

    writing_pb.maybe(|writing_pb| {
        writing_pb.finish_with_message(format!("Done in {} ms", writing_pb.elapsed().as_millis()))
    });
    println!("Compiled in {:?}", start_time.elapsed());

    Ok(())
}
