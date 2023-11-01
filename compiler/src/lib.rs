/// for #[derive()] macro in `parser.rs`
extern crate alloc;

mod ast;
mod parser;
mod scope;
#[cfg(test)]
mod tests;

use std::borrow::Cow;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;
use std::rc::Rc;
use std::time::{Duration, Instant};

use anyhow::{anyhow, bail, Context, Result};
use ast::{map_err, new_err, CompilationState};
use bytecode::compilation_bridge::{Instruction, MScriptFile, MScriptFileBuilder};
use bytecode::Program;
use indicatif::{MultiProgress, ProgressBar, ProgressFinish, ProgressStyle};

use once_cell::sync::{Lazy, OnceCell};
use parser::{AssocFileData, File as ASTFile};
use pest::Span;

use crate::ast::CompiledItem;
use crate::parser::{root_node_from_str, Parser};

#[derive(Debug)]
struct VerboseLogger {
    progress_bars: Option<MultiProgress>,
}

pub(crate) trait CompilationError {
    type Output;
    fn details(self, span: Span, file_name: &str, message: impl Into<String>) -> Self::Output;
    fn details_lazy_message(
        self,
        span: Span,
        file_name: &str,
        message: impl Fn() -> String,
    ) -> Self::Output;
}

impl<T> CompilationError for Result<T> {
    type Output = Self;
    fn details(self, span: Span, file_name: &str, message: impl Into<String>) -> Self::Output {
        map_err(self, span, file_name, message.into())
    }

    fn details_lazy_message(
        self,
        span: Span,
        file_name: &str,
        message: impl Fn() -> String,
    ) -> Self::Output {
        if self.is_err() {
            self.details(span, file_name, message())
        } else {
            self
        }
    }
}

impl<T> CompilationError for Option<T> {
    type Output = Result<T>;
    fn details(self, span: Span, file_name: &str, message: impl Into<String>) -> Self::Output {
        if let Some(x) = self {
            return Ok(x);
        }

        Err(new_err(span, file_name, message.into()))
    }

    fn details_lazy_message(
        self,
        span: Span,
        file_name: &str,
        message: impl Fn() -> String,
    ) -> Self::Output {
        if let Some(x) = self {
            Ok(x)
        } else {
            self.details(span, file_name, message())
        }
    }
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
    ) -> Result<R, Vec<anyhow::Error>> {
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

pub(crate) trait BytecodePathStr {
    fn bytecode_str(&self) -> String;
}

impl <T>BytecodePathStr for T 
where 
    T: AsRef<Path>
{
    fn bytecode_str(&self) -> String {
        self.as_ref().to_str().expect("path contains non-standard characters").replace('\\', "/")
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

impl<T, E: Into<anyhow::Error>> VecErr<T> for std::result::Result<T, E> {
    fn to_err_vec(self) -> Result<T, Vec<anyhow::Error>> {
        self.map_err(|e| vec![anyhow!(e)])
    }
}

pub fn eval(mscript_code: &str) -> Result<(), Vec<anyhow::Error>> {
    let _ = LOGGER_INSTANCE.set(VerboseLogger::new(false));

    let input_path = "$__EVAL_ENV__.ms";
    let output_path = "$__EVAL_ENV__.mmm";

    let compiled_items = compile_from_str(
        Path::new(&input_path),
        Path::new(&output_path),
        mscript_code,
    )?;

    let eval_environment: Rc<MScriptFile> =
        seal_compiled_items(Path::new(output_path), compiled_items).to_err_vec()?;

    let executable = Program::new_from_file(eval_environment).to_err_vec()?;

    executable.execute().to_err_vec()?;

    Ok(())
}

pub(crate) fn seal_compiled_items(
    output_path: &Path,
    compiled_items: Vec<CompiledItem>,
) -> Result<Rc<MScriptFile>> {
    let output_path_owned = output_path.to_string_lossy().into_owned();
    let mut file_builder = MScriptFileBuilder::new(output_path_owned);

    for compiled_item in compiled_items.into_iter() {
        let CompiledItem::Function {
            id,
            content: Some(content),
            ..
        } = compiled_item
        else {
            bail!("Encountered a non-function when packaging compiled items: {compiled_item:?}");
        };

        file_builder.add_function(
            Rc::new(id.to_string()),
            content.into_iter().map(Into::<Instruction>::into).collect(),
        );
    }

    Ok(file_builder.build())
}

pub(crate) fn ast_file_from_str(
    input_path: &Path,
    output_path: &Path,
    mscript_code: &str,
) -> Result<ASTFile, Vec<anyhow::Error>> {
    let user_data = Rc::new(AssocFileData::new(
        output_path.to_string_lossy().to_string(),
    ));

    let input_path_str = input_path.to_string_lossy();

    let input = logger().wrap_in_spinner(format!("Parsing ({input_path_str}):"), || {
        root_node_from_str(mscript_code, user_data.clone())
    })?;

    logger().wrap_in_spinner(format!("Creating AST ({input_path_str}):"), || {
        Parser::file(input)
    })
}

pub(crate) fn compile_from_str(
    input_path: &Path,
    output_path: &Path,
    mscript_code: &str,
) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
    let state: CompilationState = CompilationState::new();

    let file = ast_file_from_str(input_path, output_path, mscript_code)?;

    state.queue_compilation(file);

    let mut result = None;
    let mut c = 0;

    state.compile_recursive(&mut |src: &ASTFile, compiled_items: Vec<CompiledItem>| {
        c += 1;
        // dbg!(&src.location);
        if result.is_none() {
            #[cfg(feature = "debug")]
            perform_file_io_out(&src.location, compiled_items.clone(), false).context("The `--debug` feature flag failed to dump the HR Bytecode")?;
                
            result = Some(compiled_items);
        } else {
            perform_file_io_out(&src.location, compiled_items, true)?
        }
        Ok(())
    })?;

    // state.start_compilation();
    // let chain = CompilationStep::new(file);

    // chain.compile(&state, )?;

    // panic!("{c}");

    Ok(result.unwrap())
}

fn perform_file_io_out(
    output_path: &Path,
    function_buffer: Vec<CompiledItem>,
    output_bin: bool,
) -> Result<()> {
    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path).with_context(|| format!("Could not get file descriptor for {output_path:?}"))?;

    let writing_pb = logger().add(|| {
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
            for_each(x).with_context(|| format!("{function_buffer:#?}"))?;
        }
    } else {
        for x in iter {
            for_each(x).with_context(|| format!("{function_buffer:#?}"))?;
        }
    }

    writing_pb.maybe(|writing_pb| {
        writing_pb.finish_with_message(format!("Done in {} ms", writing_pb.elapsed().as_millis()))
    });

    new_file.flush()?;

    Ok(())
}

pub(crate) fn perform_file_io_in(input_path: &Path) -> Result<String> {
    let Some(ext) = input_path.extension() else {
        bail!("no file extension");
    };

    if ext != "ms" {
        bail!("MScript uses `.ms` file extensions. Please check your file extensions.");
    }

    let file = File::open(input_path).context("Could not open file for parsing :(")?;

    let mut reader = BufReader::new(file);

    let mut buffer = String::new();

    reader.read_to_string(&mut buffer)?;

    Ok(buffer)
}

static LOGGER_INSTANCE: OnceCell<VerboseLogger> = OnceCell::new();

pub(crate) fn logger() -> &'static VerboseLogger {
    LOGGER_INSTANCE
        .get()
        .expect("logger has not been initialized")
}

pub struct CompilationQueue {}

pub fn compile(
    input_path: impl AsRef<Path>,
    output_bin: bool,
    verbose: bool,
    output_to_file: bool,
) -> Result<Option<Rc<MScriptFile>>, Vec<anyhow::Error>> {
    LOGGER_INSTANCE
        .set(VerboseLogger::new(verbose))
        .expect("logger has already been initialized");

    let start_time = Instant::now();

    let input_path = input_path
        .as_ref();
        // .canonicalize()
        // .context("Could not get this file's path! Does it exist?")
        // .to_err_vec()?;

    let output_path = input_path.with_extension("mmm");

    let file_contents = perform_file_io_in(input_path.as_ref()).to_err_vec()?;

    let function_buffer =
        compile_from_str(input_path.as_ref(), &output_path, &file_contents)?;

    logger().wrap_in_spinner(
        format!("Optimizing ({:?}):", input_path),
        || Ok(()),
    )?;

    if verbose {
        println!("\n\nCompiled in {:?}", start_time.elapsed());
    }

    if output_to_file {
        perform_file_io_out(&output_path, function_buffer, output_bin).to_err_vec()?;
        Ok(None)
    } else {
        Ok(Some(
            seal_compiled_items(&output_path, function_buffer).to_err_vec()?,
        ))
    }
}
