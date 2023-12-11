use std::fmt::Display;

use anyhow::{bail, Result};

use crate::instruction;
use crate::parser::{Node, Parser, Rule};

use super::{r#type::IntoType, Compile, Dependencies, TypeLayout};
use super::{CompilationState, CompiledItem};

#[derive(Debug, Clone)]
pub enum Number {
    Integer(String),
    BigInt(String),
    Float(String),
    Byte(String),
}

impl Number {
    pub fn negate(&self) -> Option<Self> {
        use Number::*;
        Some(match self {
            Integer(x) => Integer("-".to_owned() + x),
            BigInt(x) => Integer("-".to_owned() + x),
            Float(x) => Float("-".to_owned() + x),
            Byte(_) => return None,
        })
    }
}

impl TryFrom<Number> for usize {
    type Error = std::num::ParseIntError;

    fn try_from(value: Number) -> std::result::Result<Self, Self::Error> {
        value.try_into()
    }
}

impl TryFrom<&Number> for usize {
    type Error = std::num::ParseIntError;

    fn try_from(value: &Number) -> std::result::Result<Self, Self::Error> {
        match value {
            Number::BigInt(bigint) => bigint.parse::<usize>(),
            Number::Integer(int) => int.parse::<usize>(),
            Number::Byte(byte) => byte.parse::<usize>(),
            Number::Float(_) => unreachable!(),
        }
    }
}

mod string_arithmetic {
    use anyhow::Result;
    use std::ops::*;

    use super::Number::{self, *};

    macro_rules! compiler_math {
        ($ty:ty: $lhs:ident $symbol:tt $rhs:ident) => {
            ($lhs.parse::<$ty>()? $symbol $rhs.parse::<$ty>()?).to_string()
        };
    }

    macro_rules! number_impl {
        (@type=$type:ty, @output=$output:ty: $op:tt as $trait:ident, $def:ident) => {
            impl $trait for $type {
                type Output = Result<$output>;
                fn $def(self, rhs: $type) -> Self::Output {
                    let matched = match (self, rhs) {
                        (Integer(x), Integer(y)) => Integer(compiler_math!(i32: x $op y)),
                        (Integer(x), Float(y)) => Float(compiler_math!(f64: x $op y)),
                        (Integer(x), BigInt(y)) => BigInt(compiler_math!(i128: x $op y)),
                        (Integer(x), Byte(y)) => Integer(compiler_math!(i32: x $op y)),
                        /*******************************/
                        (Float(x), Integer(y)) => Float(compiler_math!(f64: x $op y)),
                        (Float(x), Float(y)) => Float(compiler_math!(f64: x $op y)),
                        (Float(x), BigInt(y)) => Float(compiler_math!(f64: x $op y)),
                        (Float(x), Byte(y)) => Float(compiler_math!(f64: x $op y)),
                        /*******************************/
                        (BigInt(x), Integer(y)) => BigInt(compiler_math!(i128: x $op y)),
                        (BigInt(x), Float(y)) => Float(compiler_math!(f64: x $op y)),
                        (BigInt(x), BigInt(y)) => BigInt(compiler_math!(i128: x $op y)),
                        (BigInt(x), Byte(y)) => BigInt(compiler_math!(i128: x $op y)),
                        /*******************************/
                        (Byte(x), Integer(y)) => Integer(compiler_math!(i32: x $op y)),
                        (Byte(x), Float(y)) => Float(compiler_math!(f64: x $op y)),
                        (Byte(x), BigInt(y)) => BigInt(compiler_math!(i128: x $op y)),
                        (Byte(x), Byte(y)) => Byte(compiler_math!(u8: x $op y)),
                    };

                    Ok(matched)
                }
            }
        };
        ($op:tt as $trait:ident, $def:ident) => {
            number_impl!(@type=Number, @output=Number: $op as $trait, $def);
            number_impl!(@type=&Number, @output=Number: $op as $trait, $def);
        }
    }

    number_impl!(+ as Add, add);
    number_impl!(- as Sub, sub);
    number_impl!(* as Mul, mul);
    number_impl!(/ as Div, div);
    number_impl!(% as Rem, rem);
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Number::*;
        let str = match self {
            Integer(str) | Float(str) | Byte(str) | BigInt(str) => str.clone(),
        };

        write!(f, "{str}")
    }
}

impl Dependencies for Number {}

impl IntoType for Number {
    fn for_type(&self) -> Result<TypeLayout> {
        Ok(match self {
            Self::Byte(_) => TypeLayout::Native(super::r#type::NativeType::Byte),
            Self::Integer(_) => TypeLayout::Native(super::r#type::NativeType::Int),
            Self::BigInt(_) => TypeLayout::Native(super::r#type::NativeType::BigInt),
            Self::Float(_) => TypeLayout::Native(super::r#type::NativeType::Float),
        })
    }
}

impl Compile for Number {
    fn compile(&self, _: &CompilationState) -> Result<Vec<CompiledItem>> {
        let matched = match self {
            Number::Byte(val) => vec![instruction!(byte val)],
            Number::Float(val) => vec![instruction!(float val)],
            Number::Integer(val) => vec![instruction!(int val)],
            Number::BigInt(val) => vec![instruction!(bigint val)],
        };

        Ok(matched)
    }
}

pub fn number_from_string(string: &str, rule: Rule) -> Result<Number> {
    let as_str: String = string.chars().filter(|x| x != &'_').collect();

    let matched = match rule {
        Rule::bigint => {
            let no_prefix = &string[1..];
            if let Some(hex_part) = no_prefix.strip_prefix("0x") {
                let as_hex = i128::from_str_radix(hex_part, 16)?;
                Number::BigInt(as_hex.to_string())
            } else {
                Number::BigInt(no_prefix.to_owned())
            }
        }
        Rule::integer => Number::Integer(as_str),
        Rule::hex_int => {
            let as_hex = i128::from_str_radix(&as_str[2..], 16)?.to_string();

            Number::Integer(as_hex)
        }
        Rule::float => {
            if let Some(float_of_int) = as_str.strip_suffix(['F', 'f']) {
                Number::Float(float_of_int.to_owned())
            } else {
                Number::Float(as_str)
            }
        }
        Rule::byte => Number::Byte(as_str),
        _ => bail!("non-number rule"),
    };

    Ok(matched)
}

impl Parser {
    pub fn number(input: Node) -> Result<Number> {
        let child = input.children().next().unwrap();

        // let as_str = child.as_str().to_owned();

        number_from_string(child.as_str(), child.as_rule())
    }
}
