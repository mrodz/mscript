use std::{collections::HashMap, fmt::Display};

use crate::parser::{Node, Parser};
use anyhow::{bail, Result, Context, anyhow};
use once_cell::sync::Lazy;

use super::{function::FunctionType, math_expr::Op};

pub static mut TYPES: Lazy<HashMap<&str, TypeLayout>> = Lazy::new(|| {
    let mut x = HashMap::new();

    x.insert("bool", TypeLayout::Native(NativeType::Bool));
    x.insert("str", TypeLayout::Native(NativeType::Str));
    x.insert("int", TypeLayout::Native(NativeType::Int));
    x.insert("bigint", TypeLayout::Native(NativeType::BigInt));
    x.insert("float", TypeLayout::Native(NativeType::Float));
    x.insert("byte", TypeLayout::Native(NativeType::Byte));

    x
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeType {
    Bool,
    Str,
    Int,
    BigInt,
    Float,
    Byte,
}

impl Display for NativeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeLayout {
    Function(FunctionType),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Native(NativeType),
}

impl Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function_type) => write!(f, "{}", function_type.to_string()),
            Self::CallbackVariable(cb) => write!(f, "{}", cb.get_type_recursively().to_string()),
            Self::Native(native) => write!(f, "{}", native.to_string())
        }
    }
}

impl TypeLayout {
    pub fn can_negate(&self) -> bool {
        match self {
            Self::Native(NativeType::Str) => false,
            Self::Native(_) => true,
            _ => false
        }
    }

    pub fn get_type_recursively(&self) -> &Self {
        use TypeLayout::*;

        match self {
            CallbackVariable(cb) => cb.get_type_recursively(),
            _ => self
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
            (x, Byte, ..) => x.clone(), // byte will always get overshadowed.
            //======================
            (Str, Str | Int | BigInt | Float | Bool, Add) => Str,
            (Str, Int | BigInt, Multiply) => Str,
            //======================
            _ => return None
        };

        Some(TypeLayout::Native(matched))
    }

    #[allow(unused)]
    pub fn is_numeric(&self, allow_byte: bool) -> bool {
        let Self::Native(native) = self else {
            return false;
        };

        match native {
            NativeType::Byte if allow_byte => true,
            NativeType::BigInt | NativeType::Int | NativeType::Float => true,
            _ => false
        }
    }

    pub fn get_load_instruction(&self) -> (&'static str, u8) {
        const LOAD_CALLBACK: u8 = 0x23;
        const LOAD: u8 = 0x19;

        match self {
            Self::CallbackVariable(..) => ("load_callback", LOAD_CALLBACK),
            _ => ("load", LOAD)
        }
    }
}

pub trait IntoType {
    fn into_type(&self) -> Result<TypeLayout>;
    fn consume_for_type(self) -> Result<TypeLayout>
    where
        Self: Sized,
    {
        self.into_type()
    }
}

pub fn type_from_str(input: &str) -> Result<&'static TypeLayout> {
    unsafe {
        if let Some(r#type) = TYPES.get(input) {
            Ok(r#type)
        } else {
            bail!("type '{input}' has not been registered")
        }
    }
}

impl Parser {
    pub fn r#type(input: Node) -> Result<&'static TypeLayout> {
        let as_str = input.as_str();
        type_from_str(as_str).context(anyhow!(input.error("unknown type")))
    }
}
