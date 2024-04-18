use crate::{
    ast::{
        class::{ClassBody, ClassFlags},
        new_err,
        value::ValToUsize,
        Assignment,
    },
    parser::{AssocFileData, Node, Parser, Rule},
    scope::{ScopeReturnStatus, SuccessTypeSearchResult, TypeSearchResult},
    BytecodePathStr, CompilationError, VecErr,
};
use anyhow::{bail, Context, Result};
use pest::Span;
use std::{
    borrow::Cow,
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, RwLock},
};

use super::{
    class::ClassType,
    function::FunctionType,
    list::{ListBound, ListType},
    map::MapType,
    map_err,
    math_expr::Op,
    Compile, Dependencies, FunctionParameters, Ident, Value, WalkForType,
};

pub(crate) struct SupportedTypesWrapper(Box<[Cow<'static, TypeLayout>]>);

impl Display for SupportedTypesWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else {
            return Ok(());
        };
        write!(f, "{first}")?;

        for ty in iter {
            write!(f, ", {ty}")?;
        }

        Ok(())
    }
}

impl SupportedTypesWrapper {
    pub fn contains(&self, index: &Value, value_span: Span, file_name: &str) -> Result<bool> {
        for supported_type in self.0.iter() {
            if map_err(
                supported_type.eq_for_indexing(index),
                value_span,
                file_name,
                "the compiler will not allow this index".to_string(),
            )? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(crate) fn contains_raw_type(&self, ty: &TypeLayout) -> bool {
        self.0.contains(&Cow::Borrowed(ty))
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct StrWrapper(pub(in crate::ast) Option<usize>);

impl StrWrapper {
    pub const fn unknown_size() -> Self {
        Self(None)
    }

    pub const fn sized(len: usize) -> Self {
        Self(Some(len))
    }
}

impl From<StrWrapper> for Option<usize> {
    fn from(value: StrWrapper) -> Self {
        value.0
    }
}

impl PartialEq for StrWrapper {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            _ => true,
        }
    }
}

impl Hash for StrWrapper {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[cfg(test)]
mod eq_hash_test {
    use crate::assert_proper_eq_hash;

    use super::*;

    #[test]
    fn none_none() {
        let lhs = StrWrapper(None);
        let rhs = StrWrapper(None);

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    #[should_panic]
    fn none_some() {
        let lhs = StrWrapper(None);
        let rhs = StrWrapper(Some(12));

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    fn matching() {
        let lhs = StrWrapper(Some(0xDEADBEEF));
        let rhs = StrWrapper(Some(0xDEADBEEF));

        assert_proper_eq_hash!(lhs, rhs);
    }

    #[test]
    #[should_panic]
    fn mismatch() {
        let lhs = StrWrapper(Some(0xDEADBEEF));
        let rhs = StrWrapper(Some(0xFEEDBEEF));

        assert_proper_eq_hash!(lhs, rhs);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NativeType {
    Bool,
    Str(StrWrapper),
    Int,
    BigInt,
    Float,
    Byte,
}

impl Display for NativeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Str(..) => write!(f, "str"),
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::BigInt => write!(f, "bigint"),
            Self::Float => write!(f, "float"),
            Self::Byte => write!(f, "byte"),
        }
    }
}

#[derive(Clone, Debug, Eq)]
pub(crate) struct ModuleType {
    exported_members: Arc<RefCell<Vec<Ident>>>,
    public_types: Arc<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>,
    name: Arc<PathBuf>,
}

impl PartialEq for ModuleType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for ModuleType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl ModuleType {
    pub const fn new_initialized(
        name: Arc<PathBuf>,
        exported_members: Arc<RefCell<Vec<Ident>>>,
        public_types: Arc<RefCell<HashMap<String, Cow<'static, TypeLayout>>>>,
    ) -> Self {
        ModuleType {
            name,
            exported_members,
            public_types,
        }
    }

    pub fn get_property<'a>(&'a self, name: &str) -> Option<Ref<'a, Ident>> {
        let view = self.exported_members.borrow();
        Ref::filter_map(view, |exported_members| {
            exported_members.iter().find(|&field| field.name() == name)
        })
        .ok()
    }

    pub fn get_type<'a>(&'a self, name: &str) -> Option<Ref<'a, Cow<'static, TypeLayout>>> {
        let view = self.public_types.borrow();
        Ref::filter_map(view, |public_types| {
            public_types
                .iter()
                .find(|(this_name, _)| this_name.as_str() == name)
                .map(|x| x.1)
        })
        .ok()
    }

    pub fn name(&self) -> &Path {
        self.name.as_path()
    }

    pub fn from_node(input: &Node) -> Result<Self, Vec<anyhow::Error>> {
        assert_eq!(input.as_rule(), Rule::file);

        let mut export = input.user_data().get_export_ref();

        for child in input.children() {
            if child.as_rule() == Rule::declaration {
                let child = child
                    .children()
                    .single()
                    .expect("none or too many nodes for declaration");

                match child.as_rule() {
                    Rule::class => {
                        let mut children = child.children();

                        let mut class_flags = None;

                        let maybe_ident_node = children.next().unwrap();

                        let ident_node = if maybe_ident_node.as_rule() == Rule::ident {
                            maybe_ident_node
                        } else {
                            class_flags = Some(maybe_ident_node);
                            children.next().unwrap()
                        };

                        let class_flags = if let Some(class_flags_node) = class_flags {
                            let class_flags = Parser::class_flags(class_flags_node).to_err_vec()?;
                            log::info!("ignoring flags: {class_flags:?}");
                            Some(class_flags)
                        } else {
                            None
                        };

                        // if !class_flags.is_export() {
                        //     continue;
                        // }

                        let name = ident_node.as_str();

                        let body_node = children.next().expect("no body");

                        let ident = {
                            let _class_scope = input.user_data().push_class_unknown_self();
                            let fields = ClassBody::get_members(&body_node).to_err_vec()?;

                            let class_type = ClassType::new(
                                Arc::new(name.to_owned()),
                                fields,
                                input.user_data().bytecode_path(),
                            );

                            Ident::new(
                                name.to_owned(),
                                Some(Cow::Owned(TypeLayout::Class(class_type))),
                                true,
                            )
                        };

                        input
                            .user_data()
                            .add_type(name.into(), ident.ty().unwrap().clone());

                        if class_flags.as_ref().is_some_and(ClassFlags::is_export) {
                            log::trace!(
                                "Gen. mod {:?} -- adding class {name:?}",
                                input.user_data().source_path()
                            );

                            export.add(ident);
                        }
                    }
                    Rule::assignment => {
                        if let Ok(assignment) = Assignment::type_from_node(&child) {
                            log::trace!(
                                "Gen. mod {:?} -- adding assignment {assignment:?}",
                                input.user_data().source_path()
                            );

                            export.add(assignment)
                        }
                    }
                    Rule::type_alias => {
                        // Default behavior: always added to `AssocFileData` API
                        let ty = Parser::type_alias(child).to_err_vec()?;

                        log::trace!(
                            "Gen. mod {:?} -- adding type {ty:?}",
                            input.user_data().source_path()
                        );
                    }
                    Rule::import => {
                        log::trace!("Gen. mod @import");
                        let import = child.children().next().unwrap();

                        let import_path = import.children().last().unwrap();
                        let path = Arc::new(Parser::import_path(import_path).to_err_vec()?);

                        log::info!(
                            "++ BEGIN @import in pre-walk of {path:?} (current file: {})",
                            input.user_data().get_source_file_name()
                        );

                        let module_import = input.user_data().import(path.clone())?;

                        log::info!(
                            "-- END @import in pre-walk of {path:?} (current file: {})",
                            input.user_data().get_source_file_name()
                        );

                        if import.as_rule() == Rule::import_names {
                            let children = import.children();
                            for child in children {
                                match child.as_rule() {
                                    Rule::import_type => {
                                        let ty_node = child.children().next().unwrap();
                                        let module = module_import.module();
                                        let ty = module
                                            .get_type(ty_node.as_str())
                                            .details_lazy_message(
                                                ty_node.as_span(),
                                                &child.user_data().get_source_file_name(),
                                                || {
                                                    format!(
                                                        "`{}` has no visible type `{}`",
                                                        path.bytecode_str(),
                                                        ty_node.as_str()
                                                    )
                                                },
                                            )
                                            .to_err_vec()?;

                                        input
                                            .user_data()
                                            .add_type(ty_node.as_str().into(), ty.clone())
                                    }
                                    Rule::import_name => {
                                        let name_node = child.children().next().unwrap();
                                        let module = module_import.module();

                                        let named_export = module
                                            .get_property(name_node.as_str())
                                            .details_lazy_message(
                                                name_node.as_span(),
                                                &child.user_data().get_source_file_name(),
                                                || {
                                                    format!(
                                                        "`{}` has no visible member `{}`{}",
                                                        path.bytecode_str(),
                                                        name_node.as_str(),
                                                        TypeLayout::Module(module.clone())
                                                            .get_property_hint_from_input_no_lookup(
                                                            )
                                                    )
                                                },
                                            )
                                            .to_err_vec()?;

                                        match named_export
                                            .ty()
                                            .unwrap()
                                            .disregard_distractors(false)
                                        {
                                            TypeLayout::Class(class_type) => {
                                                input.user_data().add_type(
                                                    named_export.boxed_name(),
                                                    Cow::Owned(TypeLayout::Class(
                                                        class_type.clone(),
                                                    )),
                                                )
                                            }
                                            _ => log::trace!(
                                                "skipping import {} -- it doesn't create a type",
                                                name_node.as_str()
                                            ),
                                        }
                                    }
                                    Rule::import_path => break,
                                    other => unreachable!("{other:?}"),
                                }
                            }
                        }

                        log::trace!(
                            "Gen. mod {:?} -- @import {module_import:?}",
                            input.user_data().source_path()
                        );
                    }
                    other => log::trace!(
                        "Gen. mod {:?} -- skipping {other:?}",
                        input.user_data().source_path()
                    ),
                }
            }
        }

        log::info!(
            "DONE preloading module {}",
            input.user_data().source_path().display()
        );

        Ok(ModuleType::new_initialized(
            input.user_data().source_path(),
            export
                .exports
                .upgrade()
                .expect("exports backing ref was dropped"),
            export
                .public_types
                .upgrade()
                .expect("exports public types backing ref was dropped"),
        ))
    }
}

#[derive(Debug)]
pub(crate) struct TypeAlias;

impl Dependencies for TypeAlias {}
impl Compile for TypeAlias {
    fn compile(&self, _state: &super::CompilationState) -> Result<Vec<super::CompiledItem>> {
        Ok(vec![])
    }
}

static GENERIC_ID: RwLock<usize> = RwLock::new(0);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GenericType {
    unique_id: usize,
    stands_for: Rc<RefCell<Option<Cow<'static, TypeLayout>>>>,
}

impl Display for GenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(stands_for) = self.try_get_lock() {
            if cfg!(feature = "debug") {
                write!(f, "any.{}=", self.id())?;
            }
            write!(f, "{stands_for}")
        } else {
            write!(f, "any.{}", self.id())
        }
    }
}

impl Hash for GenericType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.unique_id.hash(state);
        if let Some(stands_for) = self.stands_for.borrow().as_ref() {
            Some(stands_for).hash(state);
        } else {
            Option::<()>::None.hash(state)
        }
    }
}

impl GenericType {
    pub fn new() -> Self {
        let mut locked = GENERIC_ID.write().unwrap();

        let ret = Self {
            unique_id: *locked,
            stands_for: Rc::new(RefCell::new(None)),
        };

        *locked += 1;

        ret
    }

    pub fn is_compatible<T>(&self, other: &TypeLayout, flags: &TypecheckFlags<T>) -> bool
    where
        T: Deref<Target = ClassType> + Debug,
    {
        let ret = if let Some(x) = self.try_get_lock() {
            x.eq_complex(other, flags)
        } else {
            self.lock_type(Cow::Owned(other.to_owned()));
            true
        };
        ret
    }

    pub const fn id(&self) -> usize {
        self.unique_id
    }

    pub fn lock_type(&self, ty: Cow<'static, TypeLayout>) {
        {
            let mut view = self.stands_for.borrow_mut();
            let ret = view.replace(ty);

            if let Some(existing) = ret {
                panic!(
                    "{existing} was already set as the type of generic #{}",
                    self.id()
                )
            }
        }
    }

    pub fn try_get_lock(&self) -> Option<Ref<Cow<'static, TypeLayout>>> {
        // let view = self.stands_for.borrow();
        Ref::filter_map(self.stands_for.borrow(), Option::as_ref).ok()
    }

    pub fn try_get_lock_mut(&self) -> Option<RefMut<Cow<'static, TypeLayout>>> {
        // let view = self.stands_for.borrow();
        RefMut::filter_map(self.stands_for.borrow_mut(), Option::as_mut).ok()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum TypeLayout {
    Function(FunctionType),
    Alias(String, Box<Cow<'static, TypeLayout>>),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Optional(Option<Box<Cow<'static, TypeLayout>>>),
    Native(NativeType),
    List(ListType),
    ValidIndexes(ListBound),
    Class(ClassType),
    Module(ModuleType),
    ClassSelf(Option<ClassType>),
    Generic(GenericType),
    Void,
    Map(MapType),
}

impl Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Alias(alias, _) => write!(f, "{alias}"),
            Self::Function(function_type) => write!(f, "{function_type}"),
            Self::CallbackVariable(cb) => {
                if cfg!(feature = "debug") {
                    write!(f, "--debug-cb:")?;
                }
                write!(f, "{}", cb.get_type_recursively())
            }
            Self::Native(native) => write!(f, "{native}"),
            Self::List(list) => write!(f, "{list}"),
            Self::ValidIndexes(upper) => write!(f, "[..{upper}]"),
            Self::Class(class_type) => write!(f, "{}", class_type.name()),
            Self::ClassSelf(..) => write!(f, "Self"),
            Self::Optional(Some(ty)) => write!(f, "{ty}?"),
            Self::Optional(None) => write!(f, "nil"),
            Self::Void => write!(f, "void"),
            Self::Generic(generic) => write!(f, "{generic}"),
            Self::Module(module) if cfg!(test) => write!(f, "<module {:?}>", module),
            Self::Module(module) => write!(f, "<module {:?}>", module.name.as_os_str()),
            Self::Map(map) => write!(f, "map[{}, {}]", map.key_type(), map.value_type()),
        }
    }
}

#[derive(Debug)]
pub(crate) struct TypecheckFlags<T>
where
    T: Deref<Target = ClassType>,
{
    executing_class: Option<T>,
    lhs_allow_optional_unwrap: bool,
    force_rhs_to_be_unwrapped_lhs: bool,
    signature_check: bool,
    enforce_str_comptime_len_if_present: bool,
}

#[allow(unused)]
impl<T> TypecheckFlags<T>
where
    T: Deref<Target = ClassType>,
{
    pub const fn classless() -> Self {
        Self {
            executing_class: None,
            lhs_allow_optional_unwrap: false,
            force_rhs_to_be_unwrapped_lhs: false,
            signature_check: false,
            enforce_str_comptime_len_if_present: false,
        }
    }

    pub const fn signature_check() -> Self {
        Self {
            executing_class: None,
            lhs_allow_optional_unwrap: false,
            force_rhs_to_be_unwrapped_lhs: false,
            signature_check: true,
            enforce_str_comptime_len_if_present: false,
        }
    }

    pub const fn use_class(class_type: Option<T>) -> Self {
        Self {
            executing_class: class_type,
            lhs_allow_optional_unwrap: false,
            force_rhs_to_be_unwrapped_lhs: false,
            signature_check: false,
            enforce_str_comptime_len_if_present: false,
        }
    }

    pub const fn lhs_unwrap(mut self, predicate: bool) -> Self {
        self.lhs_allow_optional_unwrap = predicate;
        self
    }

    pub const fn force_rhs_to_be_unwrapped_lhs(mut self, predicate: bool) -> Self {
        self.force_rhs_to_be_unwrapped_lhs = predicate;
        self
    }

    pub const fn enforce_str_comptime_len_if_present(mut self, predicate: bool) -> Self {
        self.enforce_str_comptime_len_if_present = predicate;
        self
    }
}

macro_rules! new_assoc_function {
    ($types:expr, $return_value:expr) => {
        Box::new(Box::new(Cow::Owned(TypeLayout::Function(
            FunctionType::new(
                Rc::new(FunctionParameters::TypesOnly($types)),
                $return_value,
                true,
                false,
            ),
        ))))
    };
    ($types:expr, @int) => {
        new_assoc_function!(
            $types,
            ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Int)))
        )
    };
    ($types:expr, @bigint) => {
        new_assoc_function!(
            $types,
            ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::BigInt)))
        )
    };
    ($types:expr, @byte) => {
        new_assoc_function!(
            $types,
            ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Byte)))
        )
    };
    ($types:expr, @void) => {
        new_assoc_function!($types, ScopeReturnStatus::Void)
    };
    ($types:expr, @str) => {
        new_assoc_function!($types, ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Str(StrWrapper::unknown_size())))))
    };
    ($types:expr, @float) => {
        new_assoc_function!(
            $types,
            ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Float)))
        )
    };
    (@to_str) => {
        new_assoc_function!(vec![], @str)
    };
}

impl TypeLayout {
    /// Returns whether a value is boolean. This function **does not** supply the value of the boolean.
    pub fn is_boolean(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Bool))
    }

    pub fn is_optional(&self) -> (bool, Option<&Cow<'static, TypeLayout>>) {
        let me = self.get_type_recursively();

        if let TypeLayout::Optional(x) = me {
            return (true, x.as_ref().map(|x| x.as_ref()));
        }

        (false, None)
    }

    pub fn is_map(&self) -> bool {
        let me = self.get_type_recursively();
        matches!(me, TypeLayout::Map(..))
    }

    pub fn is_float(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Float))
    }

    pub fn is_class_self(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::ClassSelf(..))
    }

    pub fn is_class(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Class(..))
            || me.is_function().is_some_and(FunctionType::is_constructor)
    }

    pub fn supports_equ(&self) -> bool {
        let me = self.get_type_recursively();

        match me {
            TypeLayout::Class(..) => false,
            TypeLayout::Function(..) => false,
            TypeLayout::Module(..) => false,
            TypeLayout::ValidIndexes(..) => unreachable!(),
            TypeLayout::Void => false,
            _ => true,
        }
    }

    pub fn get_error_hint_between_types<T>(
        &self,
        incompatible: &Self,
        class_self: Option<T>,
    ) -> Option<String>
    where
        T: Deref<Target = ClassType> + Debug,
    {
        self.get_error_hint_between_types_recursive(incompatible, class_self, 2)
    }

    fn get_error_hint_between_types_recursive<T>(
        &self,
        incompatible: &Self,
        class_self: Option<T>,
        tab_depth: usize,
    ) -> Option<String>
    where
        T: Deref<Target = ClassType> + Debug,
    {
        let class_self = class_self.as_deref();
        use NativeType::*;
        use TypeLayout::*;

        let tabs = "    ".repeat(tab_depth);

        Some(match (self, incompatible) {
            (Generic(x), y) | (y, Generic(x)) => {
                let Some(x_underlying) = x.try_get_lock() else {
                    unreachable!("a generic that has not been assigned a type should never fail a type comparison: got {x} (?), {y} (non-?)")
                };
                let maybe_extended_hint = x_underlying.get_error_hint_between_types_recursive(y, class_self, tab_depth + 1).unwrap_or_default();
                format!("\n{tabs}+ hint: this generic (unique id: {}) stands in place of {x_underlying}{maybe_extended_hint}", x.id())
            }
            (Native(BigInt), Native(Int)) | (Native(Int), Native(BigInt)) => format!("\n{tabs}+ hint: try adding 'B' before a number to convert it to a bigint, eg. `99` -> `B99` or `0x6` -> `B0x6`"),
            (Native(Byte), Native(Int | BigInt)) | (Native(Int | BigInt), Native(Byte)) => format!("\n{tabs}+ hint: try adding '0b' before a number to specify a byte literal, eg. `5` -> `0b101`"),
            (Native(Int | BigInt | Byte), Native(Float)) => format!("\n{tabs}+ hint: cast this floating point value to an integer"),
            (Native(Float), Native(Int | BigInt | Byte)) => format!("\n{tabs}+ hint: cast this integer type to a floating point"),
            (Alias(str, ty), y) | (y, Alias(str, ty)) => {
                let maybe_extended_hint = ty.get_error_hint_between_types_recursive(y, class_self, tab_depth + 1).unwrap_or_default();
                format!("\n{tabs}+ hint: `{str}` is an alias for `{ty}`{maybe_extended_hint}")
            }
            (Optional(Some(x)), Optional(Some(y))) => {
                let maybe_extended_hint = x.get_error_hint_between_types_recursive(y, class_self, tab_depth + 1).unwrap_or_default();
                format!("\n{tabs}+ hint: these optionals have differing underlying types: `{x}` is not compatible with `{y}`{maybe_extended_hint}")
            }
            (x, Optional(Some(y))) | (Optional(Some(y)), x) if x.disregard_distractors(false).eq_include_class_self(y.disregard_distractors(false)) => {
                let maybe_extended_hint = x.get_error_hint_between_types_recursive(y, class_self, tab_depth + 1).unwrap_or_default();
                format!("\n{tabs}+ hint: unwrap this optional to use its value using the `get` keyword, or provide a fallback with the `or` keyword{maybe_extended_hint}")
            }
            _ if self.disregard_distractors(true).is_class_self() || incompatible.disregard_distractors(true).is_class_self() => {
                format!("\n{tabs}+ hint: `Self` in this context means {}", class_self.map(|x| Cow::Owned(format!("`{}`", x.name()))).unwrap_or(Cow::Borrowed("a function's associated class")))
            }
            (List(l1), List(l2)) => {
                match (l1, l2) {
                    (ListType::Open(t1), ListType::Open(t2)) => {
                        let maybe_extended_hint = t1.get_error_hint_between_types_recursive(t2, class_self, tab_depth + 1).unwrap_or_default();
                        format!("\n{tabs}+ hint: these growable lists do not share the same root type: `{t1}` is not compatible with `{t2}`{maybe_extended_hint}")
                    }
                    (ListType::Mixed(types), ListType::Open(ty)) | (ListType::Open(ty), ListType::Mixed(types)) => {
                        for (i, t) in types.iter().enumerate() {
                            if !ty.eq_complex(t, &TypecheckFlags::use_class(class_self)) {
                                let maybe_extended_hint = t.get_error_hint_between_types_recursive(ty, class_self, tab_depth + 1).unwrap_or_default();

                                return Some(format!("\n{tabs}+ hint: value at index {i} has type `{t}`, which is not compatible with `{ty}`{maybe_extended_hint}"));
                            }
                        }
                        return None;
                    }
                    (ListType::Mixed(m1), ListType::Mixed(m2)) => {
                        if m1.len() != m2.len() {
                            return Some(format!("\n{tabs}+ hint: these two lists have different lengths: {} and {}", m1.len(), m2.len()));
                        }

                        for (i, (x, y)) in m1.iter().zip(m2.iter()).enumerate() {
                            if !x.eq_complex(y, &TypecheckFlags::<&ClassType>::classless()) {
                                let maybe_extended_hint = x.get_error_hint_between_types_recursive(y, class_self, tab_depth + 1).unwrap_or_default();

                                return Some(format!("\n{tabs}+ hint: type mismatch at index `{i}`: types `{x}` and `{y}` are not compatible{maybe_extended_hint}"));
                            }
                        }

                        return None;
                    }
                }
            }
            (Native(Str(..)), _) => format!("\n{tabs}+ hint: call `.to_str()` on this item to convert it into a str"),
            (Function(..), Function(..)) => format!("\n{tabs}+ hint: check the function type that you provided"),
            // (List(ListType::Mixed(types)), List(ListType::Open(..))) 
            _ => return None,
        })
    }

    pub fn is_list(&self) -> Option<&ListType> {
        let me = self.get_type_recursively();

        let TypeLayout::List(l) = me else {
            return None;
        };

        Some(l)
    }

    pub fn is_directly_callback_variable(&self) -> bool {
        matches!(self, Self::CallbackVariable(..))
    }

    pub fn has_index_length_property(&self) -> Option<ListBound> {
        match self {
            Self::List(list) => Some(list.upper_bound()),
            Self::Native(NativeType::Str(StrWrapper(Some(len)))) => Some(ListBound::Numeric(*len)),
            Self::Native(NativeType::Str(StrWrapper(None))) => Some(ListBound::Infinite),
            _ => None,
        }
    }

    pub fn owned_is_function(self) -> Option<FunctionType> {
        let me = self.get_owned_type_recursively();

        let TypeLayout::Function(f) = me else {
            return None;
        };

        Some(f)
    }

    pub fn assume_type_of_self(self, user_data: &AssocFileData) -> TypeLayout {
        if self.is_class_self() {
            TypeLayout::Class(user_data.get_type_of_executing_class().unwrap().clone())
        } else {
            self
        }
    }

    pub fn update_all_references_to_class_self(&self, class_type: ClassType) -> TypeLayout {
        match self {
            Self::Alias(name, ty) => Self::Alias(
                name.to_owned(),
                Box::new(Cow::Owned(
                    ty.update_all_references_to_class_self(class_type),
                )),
            ),
            Self::CallbackVariable(cb) => {
                Self::CallbackVariable(Box::new(cb.update_all_references_to_class_self(class_type)))
            }
            Self::Function(function) => {
                let mut new_function = function.clone();
                let (is_return_type_class_self, is_return_type_optional) = {
                    // scoped because `try_set_return_type` borrows mutably
                    let return_type = function.return_type();

                    if let Some(ty) = return_type.get_type() {
                        (
                            ty.disregard_distractors(true).is_class_self(),
                            ty.disregard_distractors(false).is_optional().0,
                        )
                    } else {
                        (false, false)
                    }
                };

                if is_return_type_class_self {
                    let mut return_type = Cow::Owned(TypeLayout::Class(class_type.clone()));

                    if is_return_type_optional {
                        return_type = Cow::Owned(TypeLayout::Optional(Some(Box::new(return_type))))
                    }

                    let new_return_status = ScopeReturnStatus::Did(return_type);

                    new_function.try_set_return_type(new_return_status);
                }

                let p = new_function.parameters_mut();

                let mut new_parameters = Vec::with_capacity(p.len());

                for ty in p.to_types().iter() {
                    let is_type_class_self = ty.disregard_distractors(true).is_class_self();
                    if is_type_class_self {
                        let is_type_optional = ty.disregard_distractors(false).is_optional().0;

                        let class_self = Cow::Owned(TypeLayout::Class(class_type.clone()));

                        let replacement = if is_type_optional {
                            Cow::Owned(TypeLayout::Optional(Some(Box::new(class_self))))
                        } else {
                            class_self
                        };

                        new_parameters.push(replacement);
                    } else {
                        new_parameters.push(ty.clone());
                    }
                }

                *p = Rc::new(FunctionParameters::TypesOnly(new_parameters));

                TypeLayout::Function(new_function)
            }
            Self::Generic(generic) => {
                if let Some(mut ty) = generic.try_get_lock_mut() {
                    let result = ty.update_all_references_to_class_self(class_type);
                    *ty = Cow::Owned(result);
                }

                TypeLayout::Generic(generic.clone())
            }
            Self::List(ListType::Mixed(types)) => {
                let mut result = Vec::with_capacity(types.len());
                for ty in types {
                    result.push(Cow::Owned(
                        ty.update_all_references_to_class_self(class_type.clone()),
                    ));
                }
                Self::List(ListType::Mixed(result))
            }
            Self::List(ListType::Open(ty)) => Self::List(ListType::Open(Box::new(Cow::Owned(
                ty.update_all_references_to_class_self(class_type),
            )))),
            Self::Optional(Some(o)) => Self::Optional(Some(Box::new(Cow::Owned(
                o.update_all_references_to_class_self(class_type),
            )))),
            Self::ClassSelf(Some(known_confirmed)) => TypeLayout::Class(known_confirmed.clone()),
            Self::ClassSelf(None) => TypeLayout::Class(class_type),
            Self::Map(map_type) => {
                let key_type = map_type
                    .key_type()
                    .update_all_references_to_class_self(class_type.clone());
                let value_type = map_type
                    .value_type()
                    .update_all_references_to_class_self(class_type);
                Self::Map(MapType::new(key_type.into(), value_type.into()))
            }
            ret @ (Self::Class(..)
            | Self::Optional(None)
            | Self::Module(..)
            | Self::Native(..)
            | Self::Void
            | Self::ValidIndexes(..)) => ret.clone(),
        }
    }

    pub fn is_callable(&self) -> Option<Cow<FunctionType>> {
        self.is_callable_allow_class(false)
    }

    pub fn is_callable_allow_class(&self, allow_class: bool) -> Option<Cow<FunctionType>> {
        match self.get_type_recursively() {
            Self::Function(f) => Some(Cow::Borrowed(f)),
            Self::Class(class_type) if allow_class => Some(Cow::Owned(class_type.constructor())),
            _ => None,
        }
    }

    pub fn is_function(&self) -> Option<&FunctionType> {
        let me = self.get_type_recursively();

        let TypeLayout::Function(f) = me else {
            return None;
        };

        Some(f)
    }

    pub fn get_property_type<'a>(
        &'a self,
        property_name: &str,
    ) -> Option<Box<dyn Deref<Target = Cow<'static, TypeLayout>> + 'a>> {
        match self.disregard_distractors(true) {
            Self::Class(class_type) => {
                let ident = class_type.get_property(property_name)?;
                let property_type: &'a Cow<'static, TypeLayout> = ident.ty().ok()?;

                Some(Box::new(property_type))
            }
            Self::Module(module_type) => {
                let ident = module_type.get_property(property_name)?;
                let property_type: Ref<'a, Cow<'static, TypeLayout>> =
                    Ref::filter_map(ident, |ident| ident.ty().ok()).ok()?;

                Some(Box::new(property_type))
            }
            Self::Generic(generic) => {
                if let Some(ty) = generic.try_get_lock() {
                    let property = ty.get_property_type(property_name)?;
                    let property = property.as_ref().deref().clone();
                    Some(Box::new(Box::new(property)))
                } else {
                    None
                }
            }
            _ if property_name == "to_str" => Some(new_assoc_function!(@to_str)),
            Self::List(list_type) => {
                match property_name {
                    "len" => return Some(new_assoc_function!(vec![], @int)),
                    "inner_capacity" => return Some(new_assoc_function!(vec![], @int)),
                    "ensure_inner_capacity" => {
                        return Some(
                            new_assoc_function!(vec![Cow::Owned(TypeLayout::Native(NativeType::Int))], @void),
                        )
                    }
                    _ => (),
                }

                if let Ok(open_list_type) =
                    list_type.try_coerce_to_open(&TypecheckFlags::<&ClassType>::classless())
                {
                    let ListType::Open(list_type) = open_list_type.as_ref() else {
                        unreachable!();
                    };

                    let list_type: Cow<'static, TypeLayout> = *list_type.clone();

                    match property_name {
                        "remove" => Some(new_assoc_function!(
                            vec![Cow::Owned(TypeLayout::Native(NativeType::Int))],
                            ScopeReturnStatus::Should(list_type)
                        )),
                        "reverse" => Some(new_assoc_function!(vec![], @void)),
                        "push" => Some(new_assoc_function!(vec![list_type], @void)),
                        "map" => {
                            let generic_constraint = GenericType::new();

                            let callback_function_parameters =
                                FunctionParameters::TypesOnly(vec![list_type]);
                            let callback_return_type = ScopeReturnStatus::Should(Cow::Owned(
                                TypeLayout::Generic(generic_constraint.clone()),
                            ));
                            let callback_type = TypeLayout::Function(FunctionType::new(
                                Rc::new(callback_function_parameters),
                                callback_return_type,
                                true,
                                false,
                            ));

                            let resulting_list = TypeLayout::List(ListType::Open(Box::new(
                                Cow::Owned(TypeLayout::Generic(generic_constraint)),
                            )));

                            Some(new_assoc_function!(
                                vec![Cow::Owned(callback_type)],
                                ScopeReturnStatus::Should(Cow::Owned(resulting_list))
                            ))
                        }
                        "filter" => {
                            let callback_function_parameters =
                                FunctionParameters::TypesOnly(vec![list_type]);
                            let callback_return_type = ScopeReturnStatus::Should(Cow::Owned(
                                TypeLayout::Native(NativeType::Bool),
                            ));
                            let callback_type = TypeLayout::Function(FunctionType::new(
                                Rc::new(callback_function_parameters),
                                callback_return_type,
                                true,
                                false,
                            ));

                            let resulting_list = TypeLayout::List(open_list_type.into_owned());

                            Some(new_assoc_function!(
                                vec![Cow::Owned(callback_type)],
                                ScopeReturnStatus::Should(Cow::Owned(resulting_list))
                            ))
                        }
                        "join" => {
                            let list_param = TypeLayout::List(open_list_type.into_owned());

                            Some(new_assoc_function!(
                                vec![Cow::Owned(list_param.clone())],
                                ScopeReturnStatus::Should(Cow::Owned(list_param))
                            ))
                        }
                        "index_of" => {
                            let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                                TypeLayout::Native(NativeType::Int),
                            ))));
                            Some(new_assoc_function!(
                                vec![list_type],
                                ScopeReturnStatus::Should(Cow::Owned(return_type))
                            ))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Self::Native(NativeType::Str(..)) => match property_name {
                "len" => Some(new_assoc_function!(vec![], @int)),
                "substring" | "delete" => {
                    let int_type = TypeLayout::Native(NativeType::Int);
                    let str_type = Cow::Owned(TypeLayout::Native(NativeType::Str(
                        StrWrapper::unknown_size(),
                    )));
                    Some(new_assoc_function!(
                        vec![Cow::Owned(int_type.clone()), Cow::Owned(int_type)],
                        ScopeReturnStatus::Should(str_type)
                    ))
                }
                "contains" => {
                    let str_type = Cow::Owned(TypeLayout::Native(NativeType::Str(
                        StrWrapper::unknown_size(),
                    )));
                    Some(new_assoc_function!(
                        vec![str_type],
                        ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Bool)))
                    ))
                }
                "index_of" => {
                    let str_type = Cow::Owned(TypeLayout::Native(NativeType::Str(
                        StrWrapper::unknown_size(),
                    )));

                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Int),
                    ))));

                    Some(new_assoc_function!(
                        vec![str_type],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "inner_capacity" => Some(new_assoc_function!(vec![], @int)),
                "reverse" => Some(new_assoc_function!(vec![], @str)),
                "insert" => {
                    let int_type = TypeLayout::Native(NativeType::Int);
                    let str_type = TypeLayout::Native(NativeType::Str(StrWrapper::unknown_size()));
                    Some(
                        new_assoc_function!(vec![Cow::Owned(str_type.clone()), Cow::Owned(int_type)], @str),
                    )
                }
                "replace" => {
                    let str_type = TypeLayout::Native(NativeType::Str(StrWrapper::unknown_size()));
                    Some(
                        new_assoc_function!(vec![Cow::Owned(str_type.clone()), Cow::Owned(str_type)], @str),
                    )
                }

                "parse_int" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Int),
                    ))));

                    Some(new_assoc_function!(
                        vec![],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_int_radix" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Int),
                    ))));

                    Some(new_assoc_function!(
                        vec![Cow::Owned(TypeLayout::Native(NativeType::Int))],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_bigint" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::BigInt),
                    ))));

                    Some(new_assoc_function!(
                        vec![],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_bigint_radix" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::BigInt),
                    ))));

                    Some(new_assoc_function!(
                        vec![Cow::Owned(TypeLayout::Native(NativeType::Int))],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_bool" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Bool),
                    ))));

                    Some(new_assoc_function!(
                        vec![],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_float" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Float),
                    ))));

                    Some(new_assoc_function!(
                        vec![],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "parse_byte" => {
                    let return_type = TypeLayout::Optional(Some(Box::new(Cow::Owned(
                        TypeLayout::Native(NativeType::Byte),
                    ))));

                    Some(new_assoc_function!(
                        vec![],
                        ScopeReturnStatus::Should(Cow::Owned(return_type))
                    ))
                }
                "split" => {
                    let str_type = TypeLayout::Native(NativeType::Str(StrWrapper::unknown_size()));
                    let return_array = TypeLayout::List(ListType::Mixed(vec![
                        Cow::Owned(str_type.clone()),
                        Cow::Owned(str_type),
                    ]));
                    Some(new_assoc_function!(
                        vec![Cow::Owned(TypeLayout::Native(NativeType::Int))],
                        ScopeReturnStatus::Should(Cow::Owned(return_array))
                    ))
                }
                _ => None,
            },
            whole_type @ Self::Native(
                variant @ (NativeType::Int
                | NativeType::BigInt
                | NativeType::Byte
                | NativeType::Float),
            ) => match (property_name, variant) {
                ("pow", NativeType::Float) => Some(
                    new_assoc_function!(vec![Cow::Owned(TypeLayout::Native(NativeType::Int))], @float),
                ),
                ("pow", ..) => Some(
                    new_assoc_function!(vec![Cow::Owned(TypeLayout::Native(NativeType::Int))], @bigint),
                ),
                ("powf", ..) => Some(
                    new_assoc_function!(vec![Cow::Owned(TypeLayout::Native(NativeType::Float))], @float),
                ),
                ("sqrt", ..) => Some(new_assoc_function!(vec![], @float)),
                ("to_int", ..) => Some(new_assoc_function!(vec![], @int)),
                ("to_bigint", ..) => Some(new_assoc_function!(vec![], @bigint)),
                ("to_byte", ..) => Some(new_assoc_function!(vec![], @byte)),
                ("to_float", ..) => Some(new_assoc_function!(vec![], @float)),
                ("abs", ..) => Some(new_assoc_function!(
                    vec![],
                    ScopeReturnStatus::Should(Cow::Owned(whole_type.to_owned()))
                )),
                ("to_ascii", NativeType::Byte) => Some(new_assoc_function!(vec![], @str)),
                ("fpart", NativeType::Float) => Some(new_assoc_function!(vec![], @float)),
                ("ipart", NativeType::Float) => Some(new_assoc_function!(vec![], @float)),
                ("round", NativeType::Float) => Some(new_assoc_function!(vec![], @float)),
                ("floor", NativeType::Float) => Some(new_assoc_function!(vec![], @float)),
                ("ceil", NativeType::Float) => Some(new_assoc_function!(vec![], @float)),
                _ => None,
            },
            Self::Function(..) => match property_name {
                "is_closure" => Some(new_assoc_function!(
                    vec![],
                    ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Native(NativeType::Bool)))
                )),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn get_addressable_properties(&self) -> Vec<String> {
        match self {
            Self::Class(class_type) => {
                let fields = class_type.arced_fields();
                let fields = fields.iter();

                let mut result = vec![];

                for field in fields {
                    result.push(field.to_string());
                }

                result
            }
            Self::Module(module_type) => {
                let fields = &module_type.exported_members;
                let view = fields.borrow();

                let mut result = vec![];

                for field in view.iter() {
                    result.push(field.to_string());
                }

                result
            }
            Self::List(list_type) => {
                let items = [
                    "fn len() -> int",
                    "fn inner_capacity() -> int",
                    "fn ensure_inner_capacity(int)",
                    "fn to_str() -> str",
                ];

                let additional = if let Ok(ListType::Open(ty)) = list_type
                    .try_coerce_to_open(&TypecheckFlags::<&ClassType>::classless())
                    .as_deref()
                {
                    Some(vec![
                        "fn reverse()".to_owned(),
                        format!("fn remove(int) -> {ty}"),
                        format!("fn push({ty})"),
                        format!("fn join([{ty}...])"),
                        format!("fn map(fn({ty}) -> K) -> [K...]"),
                        format!("fn filter(fn({ty}) -> bool) -> [{ty}...]"),
                        format!("fn index_of({ty})"),
                    ])
                } else {
                    None
                };

                items
                    .iter()
                    .map(|x| x.to_string())
                    .chain(additional.unwrap_or_default())
                    .collect::<Vec<_>>()
            }
            Self::Function(..) => {
                vec![
                    "fn is_closure() -> bool".to_owned(),
                    "fn to_str() -> str".to_owned(),
                ]
            }
            Self::Native(NativeType::Bool) => vec!["fn to_str() -> str".to_owned()],
            Self::Native(NativeType::Str(..)) => [
                "fn len() -> int",
                "fn substring(int, int) -> str",
                "fn contains(str) -> bool",
                "fn index_of(str) -> int?",
                "fn inner_capacity() -> int",
                "fn reverse() -> str",
                "fn insert(str, int) -> str",
                "fn replace(str, str) -> str",
                "fn delete(int, int) -> str",
                "fn parse_int() -> int?",
                "fn parse_int_radix(int) -> int?",
                "fn parse_bigint() -> bigint?",
                "fn parse_bigint_radix(int) -> bigint?",
                "fn parse_bool() -> bool?",
                "fn parse_float() -> float?",
                "fn parse_byte() -> byte?",
                "fn split(int) -> [str, str]",
                "fn to_str() -> str",
            ]
            .iter()
            .map(<&str>::to_string)
            .collect(),
            Self::Native(
                variant @ (NativeType::BigInt
                | NativeType::Int
                | NativeType::Float
                | NativeType::Byte),
            ) => {
                let mut shared = vec![
                    "fn powf(float) -> float",
                    "fn to_int() -> int",
                    "fn to_bigint() -> bigint",
                    "fn to_float() -> float",
                    "fn to_byte() -> byte",
                    "fn sqrt() -> float",
                    "fn abs() -> Self",
                    "fn to_str() -> str",
                ];

                match variant {
                    NativeType::Byte => {
                        shared.push("fn to_ascii() -> str");
                        shared.push("fn pow(int) -> bigint");
                    }
                    NativeType::Float => shared.extend_from_slice(&[
                        "fn pow(int) -> float",
                        "fn fpart() -> float",
                        "fn ipart() -> float",
                        "fn round() -> float",
                        "fn floor() -> float",
                        "fn ceil() -> float",
                    ]),
                    _ => shared.push("fn pow(int) -> bigint"),
                }

                shared.iter().map(<&str>::to_string).collect()
            }
            _ => vec![],
        }
    }

    pub fn get_property_hint_from_input_no_lookup(&self) -> String {
        let visible_properties = self.get_addressable_properties();

        let mut property_iter = visible_properties.iter();

        let mut result = String::new();

        if let Some(first) = property_iter.next() {
            result.push_str(first);
        }

        const LIMIT: usize = 7;

        let mut remaining = 0;

        for (idx, property) in property_iter.enumerate() {
            if idx > LIMIT {
                remaining += 1;
                continue;
            }
            result.push_str(", ");
            result.push_str(property);
        }

        if result.is_empty() {
            result.push_str("<none>")
        }

        let remaining_message = if remaining != 0 {
            Cow::Owned(format!(" (+{remaining} remaining)"))
        } else {
            Cow::Borrowed("")
        };

        let check_export_message = if matches!(self, TypeLayout::Module(_)) {
            "\n            + if you expected this item to be visible, make sure it is exported via the `export` keyword and that the module is not being imported cyclically"
        } else {
            ""
        };

        format!("\n        + available properties are: [{result}{remaining_message}]{check_export_message}")
    }

    pub fn can_be_used_as_list_index(&self) -> bool {
        matches!(
            self,
            TypeLayout::Native(NativeType::Int | NativeType::BigInt)
        )
    }

    /// - `Self` is the expected type
    /// - `rhs` is the supplied type
    pub fn eq_complex<T>(&self, rhs: &Self, flags: impl Deref<Target = TypecheckFlags<T>>) -> bool
    where
        T: Deref<Target = ClassType> + Debug,
    {
        let lhs = Cow::Borrowed(self.disregard_distractors(false));
        let rhs = Cow::Borrowed(rhs.disregard_distractors(false));

        // if let Some(executing_class) = flags.executing_class.as_ref() {
        //     lhs = Cow::Owned(lhs.update_all_references_to_class_self(executing_class.deref().clone()));
        //     rhs = Cow::Owned(rhs.update_all_references_to_class_self(executing_class.deref().clone()));
        // }

        log::debug!("lhs:{lhs:?} rhs:{rhs:?} f:{:?}", flags.deref());

        if lhs == rhs {
            return if flags.force_rhs_to_be_unwrapped_lhs {
                let x = !rhs.is_optional().0;
                log::debug!("x:{x}");
                x
            } else {
                true
            };
        }

        match (lhs.as_ref(), rhs.as_ref(), &flags.executing_class) {
            (Self::Generic(generic), other, _) | (other, Self::Generic(generic), _) => {
                generic.is_compatible(other, &flags)
            }
            (Self::ClassSelf(Some(known)), other, ..)
            | (other, Self::ClassSelf(Some(known)), ..) => {
                TypeLayout::Class(known.to_owned()).eq_complex(other, flags)
            }
            (Self::ClassSelf(None), other, Some(executing_class))
            | (other, Self::ClassSelf(None), Some(executing_class)) => {
                TypeLayout::Class(executing_class.deref().to_owned()).eq_complex(other, flags)
            }
            (Self::List(ListType::Mixed(t1)), Self::List(ListType::Mixed(t2)), _) => {
                let flags = Box::new(flags.deref());
                t1.iter()
                    .zip(t2.iter())
                    .all(|(x, y)| x.eq_complex(y, *flags))
            }
            (Self::List(ListType::Open(t1)), Self::List(ListType::Open(t2)), _) => {
                t1.eq_complex(t2, flags)
            }
            (Self::List(ListType::Mixed(t1)), Self::List(ListType::Open(t2)), _)
            | (Self::List(ListType::Open(t2)), Self::List(ListType::Mixed(t1)), _) => {
                let flags = Box::new(flags.deref());

                for ty in t1 {
                    if !t2.eq_complex(ty, *flags) {
                        return false;
                    }
                }
                true
            }
            (Self::Optional(None), ..) if flags.force_rhs_to_be_unwrapped_lhs => true,
            (Self::Optional(None), Self::Optional(Some(_)), _)
            | (Self::Optional(Some(_)), Self::Optional(None), _)
                if !flags.force_rhs_to_be_unwrapped_lhs =>
            {
                true
            }
            (Self::Optional(Some(x)), Self::Optional(Some(y)), _) => {
                x.eq_complex(y.as_ref(), flags)
            }
            (Self::Optional(Some(x)), y, _)
                if flags.force_rhs_to_be_unwrapped_lhs && !flags.signature_check =>
            {
                log::debug!("match(x:{x} y:{y})");
                x.eq_complex(y, flags)
            }
            (Self::Optional(Some(x)), y, _) if !flags.signature_check => x.eq_complex(y, flags),
            (y, Self::Optional(Some(x)), _) if flags.lhs_allow_optional_unwrap => {
                x.eq_complex(y, flags)
            }
            (Self::Native(NativeType::Str(..)), Self::Native(NativeType::Str(..)), _)
                if !flags.enforce_str_comptime_len_if_present =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn get_output_type_from_index(
        &self,
        index: &Value,
        flags: &TypecheckFlags<impl Deref<Target = ClassType> + Debug>,
    ) -> Result<Cow<TypeLayout>> {
        let me = self.disregard_distractors(false);

        if me.is_optional().0 {
            bail!("cannot index into an optional: unwrap this `{me}` first")
        }

        if let Self::Map(map) = me {
            if map.key_type().eq_complex(&index.for_type()?, flags) {}

            return Ok(Cow::Borrowed(map.value_type()));
        }

        let index_ty = index.for_type().context("could not get type of index")?;

        if !index_ty.can_be_used_as_list_index() {
            bail!("`{index_ty}` cannot be used as an index here");
        }

        let index_as_usize = index.get_usize().context("could not get index as usize")?;

        if let Self::Native(NativeType::Str(StrWrapper(maybe_length))) = me {
            match (&index_as_usize, maybe_length) {
                (ValToUsize::Ok(index), Some(str_len)) => {
                    if index >= str_len {
                        bail!("index {index} too big for str of len {str_len}")
                    }

                    let native_type = TypeLayout::Native(NativeType::Str(StrWrapper(Some(1))));

                    return Ok(Cow::Owned(native_type));
                }
                (ValToUsize::Ok(..), None) | (ValToUsize::NotConstexpr, _) => {
                    return Ok(Cow::Owned(TypeLayout::Native(NativeType::Str(StrWrapper(
                        None,
                    )))));
                }
                (ValToUsize::NaN, _) => bail!("str cannot be indexed by a non-int"),
            }
        }

        match index_as_usize {
            ValToUsize::Ok(index_as_usize) => match me {
                Self::List(list) => {
                    return Ok(Cow::Borrowed(list.get_type_at_known_index(index_as_usize)?));
                }
                _ => bail!("not indexable"),
            },
            ValToUsize::NaN => bail!(
                "List index must be an integer between {} and {:#}",
                usize::MIN,
                usize::MAX
            ),
            ValToUsize::NotConstexpr => match me {
                TypeLayout::List(ListType::Open(ty)) => return Ok(Cow::Borrowed(ty.as_ref())),
                TypeLayout::List(ListType::Mixed(ty)) => bail!("Indexing into a mixed type list ({:?}) requires that the index be evaluable at compile time", ty.iter().map(|x| x.to_string()).collect::<Vec<_>>()),
                _ => todo!()
            },
        }
    }

    pub fn supports_index(&self) -> Option<SupportedTypesWrapper> {
        let me = self.get_type_recursively();

        Some(SupportedTypesWrapper(match me {
            Self::Native(NativeType::Str(_)) => Box::new([
                Cow::Owned(TypeLayout::Native(NativeType::Int)),
                Cow::Owned(TypeLayout::Native(NativeType::BigInt)),
            ]),
            Self::List(x) => {
                let upper = x.valid_indexes();

                Box::new([Cow::Owned(TypeLayout::ValidIndexes(upper))])
            }
            Self::Map(map_type) => Box::new([Cow::Owned(map_type.key_type().to_owned())]),
            _ => return None,
        }))
    }

    pub fn supports_negate(&self) -> bool {
        let me = self.get_type_recursively();
        match me {
            Self::Native(NativeType::Str(_) | NativeType::Byte) => false,
            Self::Native(_) => true,
            _ => false,
        }
    }

    pub fn get_type_recursively(&self) -> &Self {
        use TypeLayout::*;

        match self {
            CallbackVariable(cb) => cb.get_type_recursively(),
            _ => self,
        }
    }

    pub fn disregard_optional(&self) -> Option<&Self> {
        match self {
            Self::Optional(x) => x.as_ref().map(|x| &***x),
            other => Some(other),
        }
    }

    pub fn disregard_distractors(&self, is_optional_distractor: bool) -> &Self {
        match self {
            Self::Alias(_, ty) => ty.disregard_distractors(is_optional_distractor),
            Self::CallbackVariable(x) => x.disregard_distractors(is_optional_distractor),
            Self::Optional(Some(x)) if is_optional_distractor => {
                x.disregard_distractors(is_optional_distractor)
            }
            x => x,
        }
    }

    pub fn get_owned_type_recursively(self) -> Self {
        use TypeLayout::*;

        match self {
            CallbackVariable(cb) => cb.get_owned_type_recursively(),
            _ => self,
        }
    }

    pub fn get_output_type(&self, other: &Self, op: &Op) -> Option<TypeLayout> {
        use TypeLayout::*;

        let lhs = self.disregard_distractors(false);
        let other = other.disregard_distractors(false);

        match (lhs, other) {
            (TypeLayout::Generic(g1), TypeLayout::Generic(g2)) => {
                let t1 = g1.try_get_lock()?;
                let t2 = g2.try_get_lock()?;
                return t1.get_output_type(&t2, op);
            }
            (TypeLayout::Generic(g), y) | (y, TypeLayout::Generic(g)) => {
                let x = g.try_get_lock()?;
                return x.get_output_type(y, op);
            }
            (TypeLayout::Function(f), TypeLayout::Class(class_type)) if f.is_constructor() => {
                let class_constructor = TypeLayout::Function(class_type.constructor());
                let f_as_typelayout = TypeLayout::Function(f.clone());
                return f_as_typelayout.get_output_type(&class_constructor, op);
            }
            (TypeLayout::Class(class_type), TypeLayout::Function(f)) if f.is_constructor() => {
                let class_constructor = TypeLayout::Function(class_type.constructor());
                let f_as_typelayout = TypeLayout::Function(f.clone());
                return class_constructor.get_output_type(&f_as_typelayout, op);
            }
            _ => (),
        }

        if matches!(op, Eq | Neq) && lhs == other && lhs.supports_equ() {
            return Some(TypeLayout::Native(NativeType::Bool));
        }

        if let (_, Optional(None), Eq | Neq) | (Optional(None), _, Eq | Neq) = (lhs, other, op) {
            return Some(TypeLayout::Native(NativeType::Bool));
        }

        let lhs = lhs.disregard_optional()?;
        let other = other.disregard_optional()?;

        match op {
            Op::Is => return Some(TypeLayout::Native(NativeType::Bool)),
            Op::AddAssign => return lhs.get_output_type(other, &Op::Add),
            Op::SubAssign => return lhs.get_output_type(other, &Op::Subtract),
            Op::MulAssign => return lhs.get_output_type(other, &Op::Multiply),
            Op::DivAssign => return lhs.get_output_type(other, &Op::Divide),
            Op::ModAssign => return lhs.get_output_type(other, &Op::Modulo),
            _ => (),
        }

        if let Op::Unwrap = op {
            if lhs == other {
                return Some(TypeLayout::Native(NativeType::Bool));
            }
            return None;
        }

        let Native(me) = lhs.get_type_recursively() else {
            return None;
        };

        let Native(other) = other.get_type_recursively() else {
            return None;
        };

        use NativeType::*;
        use Op::*;

        let matched = match (me, other, op) {
            (Str(..), Str(..), Eq | Neq) => Bool,
            (lhs, rhs, And | Or | Xor) => match (lhs, rhs) {
                (Bool, Bool) => Bool,
                _ => return None,
            },
            (lhs, rhs, Gt | Lt | Gte | Lte | Eq | Neq) => {
                match (lhs, rhs) {
                    (Int, Int | BigInt | Float | Byte) => Bool,
                    //======================
                    (Float, Float | Int | BigInt | Byte) => Bool,
                    //======================
                    (BigInt, BigInt | Int | Float | Byte) => Bool,
                    //======================
                    (Byte, Byte | Int | BigInt | Float) => Bool,
                    _ => return None,
                }
            }
            (lhs, rhs, BinaryXor | BinaryAnd | BinaryOr | BitwiseLs | BitwiseRs) => {
                match (lhs, rhs) {
                    (Int, Int | BigInt | Byte) => Int,
                    //======================
                    (BigInt, BigInt | Int | Byte) => BigInt,
                    //======================
                    (Byte, Byte | Int | BigInt) => Int,
                    _ => return None,
                }
            }
            (Int, Int, ..) => Int,
            (Int, BigInt, ..) => BigInt,
            (Int, Float, ..) => Float,
            //======================
            (Float, Float, ..) => Float,
            (Float, Int, ..) => Float,
            (Float, BigInt, ..) => Float,
            //======================
            (BigInt, BigInt, ..) => BigInt,
            (BigInt, Int, ..) => BigInt,
            (BigInt, Float, ..) => Float,
            //======================
            (x, Byte, ..) | (Byte, x, ..) => *x, // byte will always get overshadowed.
            //======================
            (Str(StrWrapper(Some(len1))), Str(StrWrapper(Some(len2))), Add) => {
                Str(StrWrapper(Some(len1 + len2)))
            }
            (Str(_), _, Add) => Str(StrWrapper(None)),
            (_, Str(_), Add) => Str(StrWrapper(None)),
            (Str(_), Int | BigInt, Multiply) => Str(StrWrapper(None)),
            (Int | BigInt, Str(_), Multiply) => Str(StrWrapper(None)),
            //======================
            _ => return None,
        };

        Some(TypeLayout::Native(matched))
    }

    pub fn is_numeric(&self, allow_byte: bool) -> bool {
        let Self::Native(native) = self.get_type_recursively() else {
            return false;
        };

        match native {
            NativeType::Byte if allow_byte => true,
            NativeType::BigInt | NativeType::Int | NativeType::Float => true,
            _ => false,
        }
    }

    pub(crate) fn eq_for_indexing(&self, value: &Value) -> Result<bool> {
        match self {
            Self::ValidIndexes(end) => ListBound::val_fits_between(end, value),
            other => Ok(&value.for_type()? == other),
        }
    }

    pub(crate) fn eq_include_class_self(&self, value: &Self) -> bool {
        match (self, value) {
            (TypeLayout::Class(..), TypeLayout::ClassSelf(None)) => true,
            (TypeLayout::ClassSelf(None), TypeLayout::Class(..)) => true,
            (x, y) => x.eq_complex(y, &TypecheckFlags::<&ClassType>::classless()),
        }
    }
}

pub(crate) trait IntoType {
    fn for_type(&self) -> Result<TypeLayout>;
    fn consume_for_type(self) -> Result<TypeLayout>
    where
        Self: Sized,
    {
        self.for_type()
    }
}

impl Parser {
    pub fn function_type(input: Node) -> Result<FunctionType> {
        let mut children = input.children();

        let mut child_and_rule = || {
            let child = children.next()?;

            let rule = child.as_rule();

            Some((child, rule))
        };

        let mut types = vec![];

        let mut child_and_rule_pair = child_and_rule();

        while let Some((child, Rule::r#type)) = child_and_rule_pair {
            let ty = Parser::r#type(child)?;
            types.push(ty);

            child_and_rule_pair = child_and_rule();
        }

        let return_type =
            if let Some((return_value, Rule::function_return_type)) = child_and_rule_pair {
                let ty = Parser::function_return_type(return_value)?;
                Some(ty)
            } else {
                None
            };

        let types = Rc::new(FunctionParameters::TypesOnly(types));

        Ok(FunctionType::new(
            types,
            ScopeReturnStatus::detect_should_return(return_type),
            false,
            false,
        ))
    }

    pub fn list_type_open_only(input: Node) -> Result<ListType> {
        let ty_node = input
            .children()
            .single()? // Rule: open_ended_type
            .children()
            .single()?; // Rule: type

        let ty = Self::r#type(ty_node)?;

        Ok(ListType::Open(Box::new(ty)))
    }

    pub fn list_type(input: Node) -> Result<ListType> {
        let children = input.children();

        let mut type_vec: Vec<Cow<'static, TypeLayout>> = vec![];

        for child in children {
            match child.as_rule() {
                Rule::r#type => {
                    let ty = Self::r#type(child)?;
                    type_vec.push(ty);
                }
                other_rule => unreachable!("{other_rule:?}"),
            }
        }

        Ok(ListType::Mixed(type_vec))
    }

    pub fn r#type(input: Node) -> Result<Cow<'static, TypeLayout>> {
        let mut children = input.children();

        let ty = children.next().unwrap();

        let optional_modifier = children.next();

        let span = ty.as_span();
        let file_name = input.user_data().get_source_file_name();

        use TypeLayout::*;

        const UNKNOWN_TYPE: &str = "unknown type";

        let ty_str = ty.as_str();

        let x = match ty.as_rule() {
            Rule::function_type => SuccessTypeSearchResult::Owned(Cow::Owned(Function(
                Self::function_type(ty).details(span, &file_name, UNKNOWN_TYPE)?,
            ))),
            Rule::list_type_open_only => SuccessTypeSearchResult::Owned(Cow::Owned(List(
                Self::list_type_open_only(ty).details(span, &file_name, UNKNOWN_TYPE)?,
            ))),
            Rule::list_type => SuccessTypeSearchResult::Owned(Cow::Owned(List(
                Self::list_type(ty).details(span, &file_name, UNKNOWN_TYPE)?,
            ))),
            Rule::ident => {
                let ty = input.user_data().get_type_from_str(ty_str);

                let TypeSearchResult::Ok(ty) = ty else {
                    return Err(new_err(span, &file_name, UNKNOWN_TYPE.to_owned()));
                };

                ty
            }
            x => unreachable!("{x:?} as a type hasn't been implemented"),
        };

        let x = x.into_cow();

        if optional_modifier.is_some() {
            Ok(Cow::Owned(TypeLayout::Optional(Some(Box::new(x)))))
        } else {
            Ok(x)
        }
    }

    pub fn type_alias(input: Node) -> Result<TypeAlias> {
        let mut children = input.children();

        let maybe_ident_node = children.next().unwrap();

        let (is_exported, ident_node) = if maybe_ident_node.as_rule() == Rule::ident {
            (false, maybe_ident_node)
        } else {
            (true, children.next().unwrap())
        };

        let ty_node = children.next().unwrap();
        let ty_span = ty_node.as_span();

        let mut ident = Self::ident(ident_node)?;

        let real_ty = Self::r#type(ty_node)?;

        log::trace!("formally aliasing `{}` = {real_ty}", ident.name());

        if real_ty.is_optional().0 {
            return Err(new_err(
                ty_span,
                &input.user_data().get_source_file_name(),
                "an optional qualifier is not valid in type aliases".to_owned(),
            ));
        }

        if real_ty.is_class() {
            ident.link_force_no_inherit(input.user_data(), real_ty.clone())?;
        }

        let ty = TypeLayout::Alias(ident.name().to_owned(), Box::new(real_ty));

        input
            .user_data()
            .add_type(ident.boxed_name(), Cow::Owned(ty.clone()));

        if is_exported {
            input
                .user_data()
                .get_export_ref()
                .add_type(ident.name().to_owned(), Cow::Owned(ty.clone()));
        }

        Ok(TypeAlias)
    }
}
