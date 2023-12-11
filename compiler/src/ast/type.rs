use crate::{
    ast::{class::ClassBody, new_err, value::ValToUsize, Assignment},
    parser::{AssocFileData, Node, Parser, Rule},
    scope::{ScopeReturnStatus, SuccessTypeSearchResult, TypeSearchResult},
    CompilationError,
};
use anyhow::{bail, Context, Result};
use pest::Span;
use std::{
    borrow::Cow,
    cell::{Ref, RefCell},
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
    path::PathBuf,
    rc::Rc,
    sync::Arc,
};

use super::{
    class::ClassType,
    function::FunctionType,
    list::{ListBound, ListType},
    map_err,
    math_expr::Op,
    FunctionParameters, Ident, Value, WalkForType,
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
    #[inline]
    pub const fn unknown_size() -> Self {
        Self(None)
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

// #[derive(Clone)]
// struct WeakImpl<T>(Weak<T>)
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug;

// impl <T>Debug for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug
// {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", self.0.upgrade().expect("backing was dropped when trying to debug"))
//     }
// }

// impl<T> Hash for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.deref().hash(state);
//     }
// }

// unsafe impl<T> Send for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
// }

// unsafe impl<T> Sync for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
// }

// impl<T> Eq for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
// }

// impl<T> PartialEq for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
//     fn eq(&self, other: &Self) -> bool {
//         self == other
//     }
// }

// impl<T> Deref for WeakImpl<T>
// where
//     <T as Deref>::Target: Hash,
//     T: Deref + Debug,
// {
//     type Target = T;
//     fn deref(&self) -> &Self::Target {
//         let upgraded = self
//             .0
//             .upgrade()
//             .expect("[DEREF] backing allocation was dropped");

//         unsafe {
//             &*Arc::as_ptr(&upgraded)
//         }
//     }
// }

// type ExportedMembersRaw = RefCell<Vec<Ident>>;

// #[derive(Debug, Clone)]
// #[repr(transparent)]
// struct ExportedMembers(ExportedMembersRaw);

// impl Deref for ExportedMembers {
//     type Target = [Ident];
//     fn deref(&self) -> &Self::Target {
//         // this is only meant to be used by #[derive] traits, which wouldn't borrow the RefCell's content mutably.
//         let x = unsafe {
//             self.0
//                 .try_borrow_unguarded()
//                 .expect("the backing export allocation was dropped")
//         };

//         x.as_ref()
//     }
// }

#[derive(Clone, Debug, Eq)]
pub(crate) struct ModuleType {
    exported_members: Arc<RefCell<Vec<Ident>>>,
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
        exported_members: Arc<RefCell<Vec<Ident>>>,
        name: Arc<PathBuf>,
    ) -> Self {
        // very hacky.
        ModuleType {
            exported_members,
            name,
        }
    }

    pub fn get_property<'a>(&'a self, name: &str) -> Option<Ref<'a, Ident>> {
        let view = self.exported_members.borrow();
        Ref::filter_map(view, |exported_members| {
            exported_members.iter().find(|&field| dbg!(field.name()) == dbg!(name))
        })
        .ok()
    }

    pub fn from_node(input: &Node) -> Result<Self> {
        assert_eq!(input.as_rule(), Rule::file);

        let mut export = input.user_data().get_export_ref()?;

        for child in input.children() {
            if child.as_rule() == Rule::declaration {
                let child = child
                    .children()
                    .single()
                    .expect("none or too many nodes for declaration");

                match child.as_rule() {
                    Rule::class => {
                        let mut children = child.children();

                        let class_flags;

                        let maybe_ident_node = children.next().unwrap();

                        let ident_node = if maybe_ident_node.as_rule() == Rule::ident {
                            continue;
                        } else {
                            class_flags = maybe_ident_node;
                            children.next().unwrap()
                        };

                        let class_flags = Parser::class_flags(class_flags)?;

                        if !class_flags.is_export() {
                            continue;
                        }

                        let name = ident_node.as_str();

                        let body_node = children.next().expect("no body");

                        let _class_scope = input.user_data().push_class_unknown_self();
                        let fields = ClassBody::get_members(&body_node)?;

                        let class_type = ClassType::new(
                            Arc::new(name.to_owned()),
                            fields,
                            input.user_data().bytecode_path(),
                        );

                        let ident = Ident::new(
                            name.to_owned(),
                            Some(Cow::Owned(TypeLayout::Class(class_type))),
                            true,
                        );

                        log::trace!(
                            "Gen. mod {:?} -- adding class {ident:?}",
                            input.user_data().source_path()
                        );

                        export.add(ident);
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
                    other => log::trace!(
                        "Gen. mod {:?} -- skipping {other:?}",
                        input.user_data().source_path()
                    ),
                }
            }
        }

        Ok(ModuleType::new_initialized(
            export
                .exports
                .upgrade()
                .expect("exports backing ref was dropped"),
            input.user_data().source_path(),
        ))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum TypeLayout {
    Function(FunctionType),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Optional(Option<Box<Cow<'static, TypeLayout>>>),
    Native(NativeType),
    List(ListType),
    ValidIndexes(ListBound, ListBound),
    Class(ClassType),
    Module(ModuleType),
    ClassSelf,
    Void,
}

impl Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function_type) => write!(f, "{function_type}"),
            Self::CallbackVariable(cb) => write!(f, "{}", cb.get_type_recursively()),
            Self::Native(native) => write!(f, "{native}"),
            Self::List(list) => write!(f, "{list}"),
            Self::ValidIndexes(lower, upper) => write!(f, "B({lower}..{upper})"),
            Self::Class(class_type) => write!(f, "{}", class_type.name()),
            Self::ClassSelf => write!(f, "Self"),
            Self::Optional(Some(ty)) => write!(f, "{ty}?"),
            Self::Optional(None) => write!(f, "!"),
            Self::Void => write!(f, "void"),
            Self::Module(module) => write!(f, "<module {:?}>", module.name.as_os_str()),
        }
    }
}

impl TypeLayout {
    /// Returns whether a value is boolean. This function **does not** supply the value of the boolean.
    pub fn is_boolean(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Bool))
    }

    pub fn is_optional(&self) -> (bool, Option<&Cow<'static, TypeLayout>>) {
        let me = self.get_type_recursively();

        if let TypeLayout::Optional(x @ Some(..)) = me {
            return (true, x.as_ref().map(|x| x.as_ref()));
        }

        (false, None)
    }

    pub fn is_float(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Float))
    }

    pub fn is_class_self(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::ClassSelf)
    }

    pub fn is_class(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Class(..))
    }

    pub fn get_error_hint_between_types(&self, incompatible: &Self) -> Option<&'static str> {
        use NativeType::*;
        use TypeLayout::*;
        Some(match (self, incompatible) {
            (Native(BigInt), Native(Int)) => "try adding 'B' before a number to convert it to a bigint, eg. `99` -> `B99` or `0x6` -> `B0x6`",
            (Native(Int), Native(Float)) => "cast this floating point value to an integer",
            (Native(Float), Native(Int | BigInt | Byte)) => "cast this integer type to a floating point",
            (x, Optional(Some(y))) if x == y.as_ref().as_ref() => "unwrap this optional to use its value",
            (Native(Str(..)), _) => "call `.to_str()` on this item to convert it into a str",
            (Function(..), Function(..)) => "check the function type that you provided",
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
        if matches!(self, TypeLayout::ClassSelf) {
            TypeLayout::Class(user_data.get_type_of_executing_class().unwrap().clone())
        } else {
            self
        }
    }

    pub fn is_callable(&self) -> Option<Cow<FunctionType>> {
        let me = self.get_type_recursively();

        match me {
            Self::Class(class_type) => Some(Cow::Owned(class_type.constructor())),
            Self::Function(f) => Some(Cow::Borrowed(f)),
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

    pub fn get_function(self) -> Option<FunctionType> {
        match self.get_owned_type_recursively() {
            TypeLayout::Function(f) => Some(f),
            TypeLayout::Class(class_ty) => {
                let constructor = class_ty.constructor();

                Some(FunctionType::new(
                    constructor.arced_parameters(),
                    ScopeReturnStatus::Should(Cow::Owned(TypeLayout::Class(class_ty))),
                ))

                // let params = constructor.parameters().clone();
                // let return_type = constructor.return_type();

                // let types = params.to_types();

                // let no_self = types.get(1..).unwrap_or(&[]);

                // let params = FunctionParameters::TypesOnly(no_self.into());
                // let params = Arc::new(params);

                // let return_type = return_type.clone();

                // todo!();
                // Some(FunctionType::new(params, return_type))
            }
            _ => None,
        }
    }

    pub fn get_property_type<'a>(
        &'a self,
        property_name: &str,
    ) -> Option<Box<dyn Deref<Target = Cow<'static, TypeLayout>> + 'a>> {
        match self.get_type_recursively() {
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
                // let fields = fields.iter();

                let mut result = vec![];

                for field in view.iter() {
                    result.push(field.to_string());
                }

                result
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

        format!(" Available properties are: [{result}{remaining_message}]")
    }

    pub fn can_be_used_as_list_index(&self) -> bool {
        matches!(
            self,
            TypeLayout::Native(NativeType::Int | NativeType::BigInt)
        )
    }

    /// - `Self` is the expected type
    /// - `rhs` is the supplied type
    pub fn eq_complex(
        &self,
        rhs: &Self,
        executing_class: Option<impl Deref<Target = ClassType>>,
        lhs_allow_optional_unwrap: bool,
    ) -> bool {
        if self == rhs {
            return true;
        }

        match (self, rhs, executing_class) {
            (Self::ClassSelf, TypeLayout::Class(other), Some(executing_class))
            | (TypeLayout::Class(other), Self::ClassSelf, Some(executing_class)) => {
                executing_class.deref().eq(other)
            }
            (Self::Optional(None), Self::Optional(Some(_)), _)
            | (Self::Optional(Some(_)), Self::Optional(None), _) => true,
            (Self::Optional(Some(x)), y, _) => x.as_ref().as_ref() == y,
            (y, Self::Optional(Some(x)), _) if lhs_allow_optional_unwrap => {
                x.as_ref().as_ref() == y
            }
            _ => false,
        }
    }

    pub fn get_output_type_from_index(&self, index: &Value) -> Result<Cow<TypeLayout>> {
        let me = self.get_type_recursively();

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
            ValToUsize::Ok(index_as_usize) => {
                // let index = .with_context(|| format!("List index must be an integer between {} and {:#}", usize::MIN, usize::MAX))?;

                match me {
                    Self::List(ListType::Empty) => bail!("cannot index into empty list"),
                    Self::List(list) => {
                        return Ok(Cow::Borrowed(list.get_type_at_known_index(index_as_usize)?));
                    }
                    _ => bail!("not indexable"),
                }
            }
            ValToUsize::NaN => bail!(
                "List index must be an integer between {} and {:#}",
                usize::MIN,
                usize::MAX
            ),
            ValToUsize::NotConstexpr => (),
        }

        if let Self::List(ListType::Open { types, spread, .. }) = me {
            if !types.is_empty() {
                bail!("indexing into spread-type lists with fixed-type parts requires that the index be a compile-time-constant expression")
            }

            return Ok(Cow::Owned(spread.clone().into_owned()));
        }

        bail!("cannot index")
    }

    pub fn supports_index(&self) -> Option<SupportedTypesWrapper> {
        let me = self.get_type_recursively();

        Some(SupportedTypesWrapper(match me {
            Self::Native(NativeType::Str(_)) => Box::new([
                Cow::Owned(TypeLayout::Native(NativeType::Int)),
                Cow::Owned(TypeLayout::Native(NativeType::BigInt)),
            ]),
            Self::List(x) => {
                let (lower, upper) = x.valid_indexes();

                Box::new([Cow::Owned(TypeLayout::ValidIndexes(lower, upper))])
            }
            _ => return None,
        }))
    }

    pub fn supports_negate(&self) -> bool {
        let me = self.get_type_recursively();
        match me {
            Self::Native(NativeType::Str(_)) => false,
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

    pub fn get_owned_type_recursively(self) -> Self {
        use TypeLayout::*;

        match self {
            CallbackVariable(cb) => cb.get_owned_type_recursively(),
            _ => self,
        }
    }

    pub fn get_output_type(&self, other: &Self, op: &Op) -> Option<TypeLayout> {
        use TypeLayout::*;

        match op {
            Op::AddAssign => return self.get_output_type(other, &Op::Add),
            Op::SubAssign => return self.get_output_type(other, &Op::Subtract),
            Op::MulAssign => return self.get_output_type(other, &Op::Multiply),
            Op::DivAssign => return self.get_output_type(other, &Op::Divide),
            Op::ModAssign => return self.get_output_type(other, &Op::Modulo),
            _ => (),
        }

        if self == other && matches!(op, Eq | Neq) {
            return Some(TypeLayout::Native(NativeType::Bool));
        }

        if let (_, Optional(None), Eq | Neq) | (Optional(None), _, Eq | Neq) = (self, other, op) {
            return Some(TypeLayout::Native(NativeType::Bool));
        }

        if let (yes, Optional(Some(maybe)), Eq | Neq) | (Optional(Some(maybe)), yes, Eq | Neq) =
            (self, other, op)
        {
            if yes == maybe.as_ref().as_ref() {
                return Some(TypeLayout::Native(NativeType::Bool));
            }
        }

        let Native(me) = self.get_type_recursively() else {
            return None;
        };

        let Native(other) = other.get_type_recursively() else {
            return None;
        };

        use NativeType::*;
        use Op::*;

        let matched = match (me, other, op) {
            (Str(..), Str(..), Eq | Neq) => Bool,
            (lhs, rhs, Gt | Lt | Gte | Lte | Eq | Neq) => {
                if lhs == rhs {
                    Bool
                } else {
                    match (lhs, rhs) {
                        (Int, BigInt | Float | Byte) => Bool,
                        //======================
                        (Float, Int | BigInt | Byte) => Bool,
                        //======================
                        (BigInt, Int | Float | Byte) => Bool,
                        //======================
                        (Byte, Int | BigInt | Float) => Bool,
                        _ => return None,
                    }
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
            (x, Byte, ..) => *x, // byte will always get overshadowed.
            (Byte, Int, Add) => Byte,
            //======================
            (Str(StrWrapper(Some(len1))), Str(StrWrapper(Some(len2))), Add) => {
                Str(StrWrapper(Some(len1 + len2)))
            }
            (Str(_), _, Add) => Str(StrWrapper(None)),
            (_, Str(_), Add) => Str(StrWrapper(None)),
            (Str(_), Int | BigInt, Multiply) => Str(StrWrapper(None)),
            (Int | BigInt, Str(_), Multiply) => Str(StrWrapper(None)),
            //======================
            (Bool, Bool, And | Or | Xor) => Bool,
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

    pub fn get_load_instruction(&self) -> (&'static str, u8) {
        const LOAD_CALLBACK: u8 = 0x23;
        const LOAD: u8 = 0x19;

        match self {
            Self::CallbackVariable(..) => ("load_callback", LOAD_CALLBACK),
            _ => ("load", LOAD),
        }
    }

    pub(crate) fn eq_for_indexing(&self, value: &Value) -> Result<bool> {
        match self {
            Self::ValidIndexes(start, end) => ListBound::val_fits_between(start, end, value),
            other => Ok(&value.for_type()? == other),
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
        ))
    }

    pub fn list_type_open_only(input: Node) -> Result<ListType> {
        let ty_node = input
            .children()
            .single()? // Rule: open_ended_type
            .children()
            .single()?; // Rule: type

        let ty = Self::r#type(ty_node)?;

        Ok(ListType::Open {
            types: vec![],
            spread: Box::new(ty),
            len_at_init: None,
        })
    }

    pub fn list_type(input: Node) -> Result<ListType> {
        let children = input.children();

        let mut type_vec: Vec<Cow<'static, TypeLayout>> = vec![];
        let mut open_ended_type: Option<Cow<'static, TypeLayout>> = None;

        for child in children {
            match child.as_rule() {
                Rule::r#type => {
                    let ty = Self::r#type(child)?;

                    type_vec.push(ty);
                }
                Rule::open_ended_type if open_ended_type.is_none() => {
                    let ty_node = child.children().single().unwrap();
                    let ty = Self::r#type(ty_node)?;

                    open_ended_type = Some(ty);
                }
                other_rule => unreachable!("{other_rule:?}"),
            }
        }

        if type_vec.is_empty() && open_ended_type.is_none() {
            return Ok(ListType::Empty);
        }

        if let Some(open_ended_type) = open_ended_type {
            return Ok(ListType::Open {
                types: type_vec,
                spread: Box::new(open_ended_type),
                len_at_init: None,
            });
        }

        Ok(ListType::Mixed(type_vec))
    }

    pub fn r#type(input: Node) -> Result<Cow<'static, TypeLayout>> {
        let mut children = input.children();

        let ty = children.next().unwrap();

        // let ty = input.children().single().unwrap();

        let optional_modifier = children.next();

        let span = ty.as_span();
        let file_name = input.user_data().get_source_file_name();

        use TypeLayout::*;

        const UNKNOWN_TYPE: &str = "unknown type";

        let x =
            match ty.as_rule() {
                Rule::function_type => SuccessTypeSearchResult::Owned(Function(
                    Self::function_type(ty).details(span, &file_name, UNKNOWN_TYPE)?,
                )),
                Rule::list_type_open_only => SuccessTypeSearchResult::Owned(List(
                    Self::list_type_open_only(ty).details(span, &file_name, UNKNOWN_TYPE)?,
                )),
                Rule::list_type => SuccessTypeSearchResult::Owned(List(
                    Self::list_type(ty).details(span, &file_name, UNKNOWN_TYPE)?,
                )),
                Rule::ident => {
                    let ty = input.user_data().get_type_from_str(ty.as_str());

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
}
