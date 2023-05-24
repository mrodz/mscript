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

// pub enum Types {
// 	Int = "int",
// 	Float = "float"
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeType {
    Bool,
    Str,
    Int,
    BigInt,
    Float,
    Byte,
    Function(Vec<NativeType>),
    Vector,
    Object(HashMap<String, NativeType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeLayout {
    Function(FunctionType),
    Standard(String),
    Native(NativeType),
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

impl Parser {
    pub fn r#type(input: Node) -> Result<&'static TypeLayout> {
        let as_str = input.as_str();

        unsafe {
            if let Some(r#type) = TYPES.get(as_str) {
                Ok(r#type)
            } else {
                bail!("type {as_str} has not been registered")
            }
        }
    }
}
