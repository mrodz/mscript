/// for #[derive()] macro in `parser.rs`
extern crate alloc;

mod ast;
mod parser;
mod scope;
#[cfg(test)]
mod tests;

use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::{anyhow, bail, Context, Result};
use ast::{map_err, new_err, CompilationState, ModuleType};
use bytecode::compilation_bridge::{Instruction, MScriptFile, MScriptFileBuilder};
use bytecode::Program;
use indicatif::{MultiProgress, ProgressBar, ProgressFinish, ProgressStyle};

use once_cell::sync::{Lazy, OnceCell};
use parser::{AssocFileData, File as ASTFile, Node};
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

impl<T> BytecodePathStr for T
where
    T: AsRef<Path>,
{
    fn bytecode_str(&self) -> String {
        self.as_ref()
            .to_str()
            .expect("path contains non-standard characters")
            .replace('\\', "/")
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

#[derive(Debug, Clone)]
pub(crate) struct FileManager {
    comptime_preloaded: Option<Rc<RefCell<HashMap<&'static str, &'static str>>>>,
    loaded_modules: Rc<RefCell<HashMap<Arc<PathBuf>, ModuleType>>>,
    completed_ast: Rc<RefCell<HashMap<Arc<PathBuf>, ASTFile>>>,
}

impl PartialEq for FileManager {
    fn eq(&self, other: &Self) -> bool {
        let v1 = self.completed_ast.borrow();
        let v2 = other.completed_ast.borrow();

        let k1: HashSet<String> = v1.keys().map(|x| x.bytecode_str()).collect();
        let k2: HashSet<String> = v2.keys().map(|x| x.bytecode_str()).collect();

        self.comptime_preloaded == other.comptime_preloaded
            && self.loaded_modules == other.loaded_modules
            && k1 == k2
    }
}

impl Display for FileManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t - Static Preloads: ")?;
        if let Some(preloaded) = &self.comptime_preloaded {
            let view = preloaded.borrow();
            write!(f, "{:?}", view.keys())?
        } else {
            write!(f, "<none>")?
        }

        let modules = self.loaded_modules.borrow();
        let ast = self.completed_ast.borrow();

        write!(
            f,
            "\n\t - Loaded Modules: {:?}\n\t - AST: {:?}",
            modules.keys(),
            ast.keys()
        )
    }
}

impl FileManager {
    pub fn new() -> Self {
        Self::new_with_capacity(1)
    }

    pub fn no_mock() -> Self {
        Self {
            comptime_preloaded: None,
            loaded_modules: Rc::default(),
            completed_ast: Rc::default(),
        }
    }

    pub fn new_with_capacity(capacity: usize) -> Self {
        Self {
            comptime_preloaded: Some(Rc::new(RefCell::new(HashMap::with_capacity(capacity)))),
            loaded_modules: Rc::default(),
            completed_ast: Rc::default(),
        }
    }

    pub fn register_preloaded_file(
        &self,
        file: &'static str,
        content: &'static str,
    ) -> Option<&'static str> {
        self.comptime_preloaded.as_ref().expect("this instance was initialized as a `no-mock`, which means file registration is incorrect behavior").borrow_mut().insert(file, content)
    }

    pub fn get_preloaded_file(&self, path: &str) -> Option<&'static str> {
        self.comptime_preloaded.as_ref().and_then(|map_view| {
            let map_view = map_view.borrow();
            map_view.get(path).cloned()
        })
    }

    pub fn preloaded_file_exists(&self, path: &str) -> bool {
        self.comptime_preloaded
            .as_ref()
            .map(|map_view| {
                let map_view = map_view.borrow();
                map_view.contains_key(path)
            })
            .unwrap_or_default()
    }

    pub fn get_module_type(&self, path: &PathBuf) -> Option<Ref<ModuleType>> {
        Ref::filter_map(self.loaded_modules.borrow(), |loaded_modules| {
            loaded_modules.get(path)
        })
        .ok()
    }

    pub fn register_module(&self, path: Arc<PathBuf>, module: ModuleType) -> Ref<ModuleType> {
        {
            let mut view = self.loaded_modules.borrow_mut();

            if let Some(prev) = view.insert(path.clone(), module) {
                panic!("this module was already cached (prev: {prev:?})");
            }
        }

        Ref::map(self.loaded_modules.borrow(), |modules| {
            modules.get(&path).unwrap()
        })

        // RefMut::filter_map(self.loaded_modules.borrow_mut(), f)
    }

    pub fn get_ast_file(&self, path: &PathBuf) -> Option<Ref<ASTFile>> {
        log::info!("polling compilation of `{path:?}`");
        Ref::filter_map(self.completed_ast.borrow(), |completed_ast| {
            completed_ast.get(path)
        })
        .ok()
    }

    pub fn register_ast(&self, path: Arc<PathBuf>, file: ASTFile) -> Ref<ASTFile> {
        log::info!("registering completion of {path:?}");

        {
            let mut view = self.completed_ast.borrow_mut();

            if let Some(prev) = view.insert(path.clone(), file) {
                panic!("this ast file was already cached (prev: {prev:?})");
            }
        }

        Ref::map(self.completed_ast.borrow(), |completed_ast| {
            completed_ast.get(&path).unwrap()
        })
    }
}

#[derive(Debug)]
pub struct EvalEnvironment {
    entrypoint: &'static str,
    files: Option<HashMap<&'static str, &'static str>>,
}

impl EvalEnvironment {
    fn validate_path(file_name: &'static str) -> Result<()> {
        let input_path = Path::new(file_name);

        if input_path.extension().expect("no extension") != "ms" {
            panic!("files must have the .ms extension in this mock context")
        }

        Ok(())
        // let compiled_items = compile_from_str(input_path, output_path.as_path(), mscript_code)?;

        // seal_compiled_items(output_path.as_path(), compiled_items).to_err_vec()
    }

    pub fn entrypoint(file_name: &'static str, code: &'static str) -> Result<Self> {
        let _ = LOGGER_INSTANCE.set(VerboseLogger::new(false));

        Self::validate_path(file_name)?;

        Ok(Self {
            entrypoint: file_name,
            files: Some(HashMap::from([(file_name, code)])),
        })
    }

    pub fn add(&mut self, file_name: &'static str, code: &'static str) -> Result<&mut Self> {
        Self::validate_path(file_name)?;

        let Some(ref mut hashmap) = self.files else {
            unreachable!("attempting to add a file to a project without an entrypoint")
        };

        if hashmap.insert(file_name, code).is_some() {
            bail!("`{file_name}` has already been loaded into this environment, which is assumed to be an error.")
        }

        Ok(self)
    }

    pub fn run(&mut self) -> Result<(), Vec<anyhow::Error>> {
        let files = self.files.take().expect("no files registered");

        let entrypoint_code = files
            .get(&self.entrypoint)
            .expect("entrypoint code does not exist");

        let file_tracker = FileManager::new();

        for (input_path, code) in files.iter() {
            // let bytecode_file = compile_from_str(input_path, output_path, code, file_tracker)?;
            // let sealed = seal_compiled_items(output_path, bytecode_file).to_err_vec()?;
            file_tracker.register_preloaded_file(input_path, code);
        }

        let output_path = Path::new(self.entrypoint).with_extension("mmm");

        let program =
            compile_from_str(self.entrypoint, output_path, entrypoint_code, file_tracker)?;

        program.execute().to_err_vec()
    }
}

pub fn eval(mscript_code: &'static str) -> Result<(), Vec<anyhow::Error>> {
    EvalEnvironment::entrypoint("$__EVAL_ENV__.ms", mscript_code)
        .to_err_vec()?
        .run()?;
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

pub(crate) fn root_ast_from_str(
    input_path: impl AsRef<Path>,
    output_path: impl AsRef<Path>,
    mscript_code: &str,
    files_loaded: FileManager,
) -> Result<Node, Vec<anyhow::Error>> {
    let input_path = input_path.as_ref();
    let output_path = output_path.as_ref();

    let user_data = Rc::new(AssocFileData::new(
        output_path.to_string_lossy().to_string(),
        files_loaded,
    ));

    let input_path_str = input_path.bytecode_str();

    logger().wrap_in_spinner(format!("Parsing ({input_path_str}):"), || {
        root_node_from_str(mscript_code, user_data.clone())
    })
}

pub(crate) fn ast_file_from_str(node: Node, input_path: &str) -> Result<(), Vec<anyhow::Error>> {
    logger().wrap_in_spinner(format!("Creating AST ({input_path}):"), || {
        Parser::file(node)
    })
}

/// This function does not perform any I/O. Instead, it loads the entire bytecode executable program
/// into memory on the heap and runs it. While this approach is faster than using files, it is not worth
/// doing for large projects.
///
/// Bytecode files are loaded lazily, so if a source file is only used later on in a program's lifetime, it
/// will only be loaded when it is needed. Using this function bypasses the default behavior by ensuring every
/// single dependent bytecode source is loaded **before** execution.
///
/// This behavior could be desired for mock environments, unit testing, and REPL environments, to name a few.
///
/// For standard behavior, see [`compile_from_str_default_side_effects`]
pub(crate) fn compile_from_str(
    input_path: impl AsRef<Path>,
    output_path: impl AsRef<Path>,
    mscript_code: &str,
    files_loaded: FileManager,
) -> Result<Program, Vec<anyhow::Error>> {
    let state: CompilationState = CompilationState::new();

    let node = root_ast_from_str(
        input_path.as_ref(),
        output_path,
        mscript_code,
        files_loaded.clone(),
    )?;

    ast_file_from_str(node, &input_path.bytecode_str())?;

    state.queue_compilation(input_path.as_ref().into());

    let mut files_in_use = HashMap::new();

    let mut shared_ptr_entrypoint_path = None;

    state.compile_recursive(&files_loaded, &mut |src, compiled_items| {
        let compiled = seal_compiled_items(&src.location, compiled_items)?;

        // Check: is this the first file compiled?
        if shared_ptr_entrypoint_path.is_none() {
            shared_ptr_entrypoint_path = Some(compiled.path_shared())
        }

        files_in_use.insert(compiled.path_shared(), compiled);

        Ok(())
    })?;

    Program::new_from_files(
        shared_ptr_entrypoint_path.expect("no files were compiled"),
        files_in_use,
    )
    .to_err_vec()
}

pub(crate) fn compile_from_str_default_side_effects(
    input_path: impl AsRef<Path>,
    output_path: impl AsRef<Path>,
    mscript_code: &str,
    files_loaded: FileManager,
) -> Result<Vec<CompiledItem>, Vec<anyhow::Error>> {
    let state: CompilationState = CompilationState::new();

    let node = root_ast_from_str(
        input_path.as_ref(),
        output_path,
        mscript_code,
        files_loaded.clone(),
    )?;
    ast_file_from_str(node, &input_path.bytecode_str())?;

    state.queue_compilation(input_path.as_ref().into());

    let mut result = None;

    state.compile_recursive(
        &files_loaded,
        &mut |src: &ASTFile, compiled_items: Vec<CompiledItem>| {
            if result.is_none() {
                #[cfg(feature = "debug")]
                perform_file_io_out(&src.location, &compiled_items, false)
                    .context("The `--debug` feature flag failed to dump the HR Bytecode")?;

                result = Some(compiled_items.clone());
            } else {
                perform_file_io_out(&src.location, &compiled_items, true)?
            }
            Ok(())
        },
    )?;

    Ok(result.unwrap())
}

fn perform_file_io_out(
    output_path: &Path,
    function_buffer: &Vec<CompiledItem>,
    output_bin: bool,
) -> Result<()> {
    let mut new_file = File::options()
        .create(true)
        .read(true)
        .write(true)
        .truncate(true)
        .open(output_path)
        .with_context(|| format!("Could not get file descriptor for {output_path:?}"))?;

    let writing_pb = logger().add(|| {
        let template = "[{elapsed_precise:.bold.green}] {prefix:.bold.dim} {msg:.yellow}";
        let style = ProgressStyle::with_template(template)
            .unwrap()
            .progress_chars("=> ");

        ProgressBar::new_spinner()
            .with_prefix(format!("Writing ({})", output_path.bytecode_str()))
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
    override_no_pb: bool,
) -> Result<Option<Rc<MScriptFile>>, Vec<anyhow::Error>> {
    LOGGER_INSTANCE
        .set(VerboseLogger::new(verbose && !override_no_pb))
        .expect("logger has already been initialized");

    let start_time = Instant::now();

    let input_path = input_path.as_ref();
    // .canonicalize()
    // .context("Could not get this file's path! Does it exist?")
    // .to_err_vec()?;

    let output_path = input_path.with_extension("mmm");

    let file_contents = perform_file_io_in(input_path).to_err_vec()?;

    let function_buffer = compile_from_str_default_side_effects(
        input_path,
        &output_path,
        &file_contents,
        FileManager::no_mock(),
    )?;

    logger().wrap_in_spinner(
        format!("Optimizing ({}):", input_path.bytecode_str()),
        || Ok(()),
    )?;

    if verbose {
        if !override_no_pb {
            print!("\n\n");
        }
        println!("Compiled in {:?}\n", start_time.elapsed());
    }

    if output_to_file {
        perform_file_io_out(&output_path, &function_buffer, output_bin).to_err_vec()?;
        Ok(None)
    } else {
        Ok(Some(
            seal_compiled_items(&output_path, function_buffer).to_err_vec()?,
        ))
    }
}
