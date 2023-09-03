use std::{borrow::Cow, fmt::Display, hash::Hash, sync::Arc};

use crate::{
    ast::value::ValToUsize,
    instruction,
    parser::{Node, Parser, Rule},
    scope::{ScopeReturnStatus, SuccessTypeSearchResult, TypeSearchResult},
    CompilationError,
};
use anyhow::{bail, Result};
use pest::Span;

use super::{
    class::ClassType,
    function::FunctionType,
    list::{ListBound, ListType},
    map_err,
    math_expr::Op,
    CompiledItem, FunctionParameters, Value, Ident,
};

pub(crate) struct SupportedTypesWrapper(Box<[Cow<'static, TypeLayout>]>);

pub(crate) static BIGINT_TYPE: TypeLayout = TypeLayout::Native(NativeType::BigInt);
pub(crate) static BOOL_TYPE: TypeLayout = TypeLayout::Native(NativeType::Bool);
pub(crate) static BYTE_TYPE: TypeLayout = TypeLayout::Native(NativeType::Byte);
pub(crate) static FLOAT_TYPE: TypeLayout = TypeLayout::Native(NativeType::Float);
pub(crate) static INT_TYPE: TypeLayout = TypeLayout::Native(NativeType::Int);
pub(crate) static STR_TYPE: TypeLayout =
    TypeLayout::Native(NativeType::Str(StrWrapper::unknown_size()));
pub(crate) static SELF_TYPE: TypeLayout = TypeLayout::ClassSelf;

impl Display for SupportedTypesWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else {
                            return Ok(())
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum TypeLayout {
    Function(FunctionType),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Native(NativeType),
    List(ListType),
    ValidIndexes(ListBound, ListBound),
    Class(ClassType),
    ClassSelf,
}

impl Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function_type) => write!(f, "{function_type}"),
            Self::CallbackVariable(cb) => write!(f, "{}", cb.get_type_recursively()),
            Self::Native(native) => write!(f, "{native}"),
            Self::List(list) => write!(f, "{list}"),
            Self::ValidIndexes(lower, upper) => write!(f, "B({lower}..{upper})"),
            Self::Class(class_type) => write!(f, "{class_type}"),
            Self::ClassSelf => write!(f, "self"),
        }
    }
}

impl TypeLayout {
    /// Returns whether a value is boolean. This function **does not** supply the value of the boolean.
    pub fn is_boolean(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Bool))
    }

    pub fn is_float(&self) -> bool {
        let me = self.get_type_recursively();

        matches!(me, TypeLayout::Native(NativeType::Float))
    }

    pub fn get_error_hint_between_types(&self, incompatible: &Self) -> Option<&'static str> {
        use NativeType::*;
        use TypeLayout::*;
        Some(match (self, incompatible) {
            (Native(BigInt), Native(Int)) => "try adding 'B' before a number to convert it to a bigint, eg. `99` -> `B99` or `0x6` -> `B0x6`",
            (Native(Int), Native(Float)) => "cast this floating point value to an integer",
            (Native(Float), Native(Int | BigInt | Byte)) => "cast this integer type to a floating point",
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

    pub fn is_function(&self) -> Option<&FunctionType> {
        let me = self.get_type_recursively();

        let TypeLayout::Function(f) = me else {
            return None;
        };

        Some(f)
    }

    pub fn get_function(self) -> Option<FunctionType> {
        let me = self.get_owned_type_recursively();

        let TypeLayout::Function(f) = me else {
            return None;
        };

        Some(f)
    }

    pub fn get_property_type<'a>(&'a self, property_name: &str) -> Option<&'a TypeLayout> {
        match self {
            Self::Class(class_type) => {
                let ident = class_type.get_property(property_name)?;
                let property_type: &'a Cow<'static, TypeLayout> = ident.ty().ok()?;

                Some(&property_type)
            }
            _ => None,
        }
    }

    pub fn can_be_used_as_list_index(&self) -> bool {
        matches!(
            self,
            TypeLayout::Native(NativeType::Int | NativeType::BigInt)
        )
    }

    pub fn get_output_type_from_index(&self, index: &Value) -> Result<Cow<TypeLayout>> {
        let me = self.get_type_recursively();

        let index_ty = index.for_type()?;

        if !index_ty.can_be_used_as_list_index() {
            bail!("`{index_ty}` cannot be used as an index here");
        }

        let index_as_usize = index.get_usize()?;

        if let Self::Native(NativeType::Str(StrWrapper(Some(str_len)))) = me {
            match index_as_usize {
                ValToUsize::Ok(index) => {
                    if index >= *str_len {
                        bail!("index {index} too big for str of len {str_len}")
                    }

                    let native_type = TypeLayout::Native(NativeType::Str(StrWrapper(Some(1))));

                    return Ok(Cow::Owned(native_type));
                }
                ValToUsize::NotConstexpr => (),
                ValToUsize::NaN => bail!("str cannot be indexed by a non-int"),
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

        let Native(me) = self.get_type_recursively() else {
            return None;
        };

        let Native(other) = other.get_type_recursively() else {
            return None
        };

        use NativeType::*;
        use Op::*;

        let matched = match (me, other, op) {
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

// pub(crate) fn type_from_str(
//     input: &str,
//     user_data: Rc<AssocFileData>,
// ) -> Result<Cow<'static, TypeLayout>> {
//     unsafe {
//         if let Some(r#type) = user_data.get_type(input) {
//             Ok(Cow::Borrowed(r#type))
//         } else {
//             if let Ok(list_type_open_only) =
//                 parse_with_userdata_features(Rule::list_type_open_only, input, user_data.clone())
//             {
//                 let single = list_type_open_only.single()?;
//                 let ty = Parser::list_type_open_only(single)?;
//                 return Ok(Cow::Owned(TypeLayout::List(ty)));
//             } else if let Ok(function_type) =
//                 parse_with_userdata_features(Rule::function_type, input, user_data.clone())
//             {
//                 let single = function_type.single()?;
//                 let ty = Parser::function_type(single)?;
//                 return Ok(Cow::Owned(TypeLayout::Function(Arc::new(ty))));
//             } else if let Ok(list_type) =
//                 parse_with_userdata_features(Rule::list_type, input, user_data)
//             {
//                 let single = list_type.single()?;
//                 let ty = Parser::list_type(single)?;
//                 return Ok(Cow::Owned(TypeLayout::List(ty)));
//             }

//             bail!("type '{input}' has not been registered")
//         }
//     }
// }

impl Parser {
    pub fn function_type(input: Node) -> Result<FunctionType> {
        let mut children = input.children();

        let mut child_and_rule = || {
            let Some(child) = children.next() else {
                return None
            };

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

        let types = Arc::new(FunctionParameters::TypesOnly(types));

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
        let ty = input.children().single().unwrap();

        let span = ty.as_span();
        let file_name = input.user_data().get_file_name();

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
                        bail!("unknown type")
                    };

                    ty
                }
                x => unreachable!("{x:?} as a type hasn't been implemented"),
            };

        let x = x.into_cow();

        Ok(x)
    }
}
