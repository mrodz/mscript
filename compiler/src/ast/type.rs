use std::collections::HashMap;

use crate::parser::{Node, Parser};
use anyhow::{bail, Result};
use once_cell::sync::Lazy;

use super::function::FunctionType;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeLayout {
    Function(FunctionType),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Native(NativeType),
}

impl TypeLayout {
    pub fn can_negate(&self) -> bool {
        match self {
            Self::Native(NativeType::Str) => false,
            Self::Native(_) => true,
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
    fn into_type(&self) -> TypeLayout;
    fn consume_for_type(self) -> TypeLayout
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
            bail!("type {input} has not been registered")
        }
    }
}

impl Parser {
    pub fn r#type(input: Node) -> Result<&'static TypeLayout> {
        let as_str = input.as_str();
        type_from_str(as_str)
    }
}
