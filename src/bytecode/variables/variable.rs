use std::fmt::{Debug, Display};

use anyhow::{bail, Result};

use crate::bytecode::variables::{variable, self};

macro_rules! primitive {
    ($($variant:ident = $type:ty)+) => {
        pub mod buckets {
            $(
                #[derive(PartialEq, PartialOrd, Clone)]
                pub struct $variant(pub(in crate) $type);

                impl std::fmt::Debug for $variant {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                        write!(f, "{}", self.0)
                    }  
                }

                impl std::fmt::Display for $variant {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                        write!(f, "{}", self.0)
                    }  
                }

                impl std::ops::Deref for $variant {
                    type Target = $type;
                    fn deref(&self) -> &Self::Target { 
                        &self.0
                    }
                }
            )*
        }

        #[derive(PartialEq, PartialOrd, Debug, Clone)]
        pub enum Primitive {
            $(
                $variant(buckets::$variant),
            )*
        }

        #[derive(Debug, Eq, PartialEq)]
        pub enum Type {
            $(
                $variant,
            )*
        }

        impl Primitive {
            pub fn ty(&self) -> Type {
                match self {
                    $(
                        Primitive::$variant(_) => Type::$variant,
                    )*
                }
            }

            #[allow(unused)]
            pub fn raw_size(&self) -> usize {
                match self {
                    $(
                        Primitive::$variant(_) => std::mem::size_of::<$type>(),
                    )*
                }
            }
        }
    };
}

primitive! {
    Bool = bool
    Str = String
    Int = i32
    BigInt = i128
    Float = f64
    Char = char
    Byte = u8
}

pub trait LooseEq: PartialEq {
    fn eq(&self, other: &Self) -> bool;
}

// impl LooseEq for Primitive {
//     fn eq(&self, other: &Self) -> bool {
    
//     }
// }

pub struct Variable {
    pub data: Primitive,
    pub ty: Type,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({})", self.ty, self.data)
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

impl Variable {
    pub fn new(data: Primitive) -> Self {
        Self {
            ty: data.ty(),
            data,
        }
    }
}

#[allow(unused)]
impl Primitive {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Int(_) | Self::Float(_) | Self::Byte(_) | Self::BigInt(_)
        )
    }

    pub fn is_numeric_coalesce(&self) -> bool {
        use Primitive::*;

        matches!(self, Bool(_) | Int(_) | Float(_) | Char(_) | Byte(_))
    }

    pub fn make_str(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if let (Some(b'\"'), Some(b'\"')) = (bytes.first(), bytes.last()) {
            return Ok(Primitive::Str(buckets::Str(string[1..string.len() - 1].to_string())));
        }

        bail!("not a Str")
    }

    pub fn make_byte(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() >= 3 && &bytes[..2] == [b'0', b'b'] {
            let byte =
                u8::from_str_radix(&string[2..], 2).expect("malformed byte. format: (0b10101010)");

            return Ok(Primitive::Byte(buckets::Byte(byte)));
        }

        bail!("not a Byte")
    }

    pub fn make_char(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() == 3 && bytes[0] == b'\'' && bytes[2] == b'\'' {
            return Ok(Primitive::Char(buckets::Char(bytes[1].into())));
        }

        bail!("not a Char")
    }

    pub fn make_float(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = f64::from_str(&string) {
            return Ok(Primitive::Float(buckets::Float(is_f64)));
        }

        bail!("not a Float")
    }

    pub fn make_int(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_i64) = i32::from_str(&string) {
            return Ok(Primitive::Int(buckets::Int(is_i64)));
        }

        bail!("not an Int")
    }

    pub fn make_bigint(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = i128::from_str(&string) {
            return Ok(Primitive::BigInt(buckets::BigInt(is_f64)));
        }

        bail!("not a BigInt")
    }

    pub fn make_bool(string: &str) -> Result<Self> {
        match string {
            "true" => Ok(Primitive::Bool(buckets::Bool(true))),
            "false" => Ok(Primitive::Bool(buckets::Bool(false))),
            _ => bail!("not a Bool"),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Primitive::*;

        match self {
            Bool(b) => write!(f, "{}", *b),
            Str(s) => write!(f, "{s:?}"),
            Int(n) => write!(f, "{n}"),
            BigInt(n) => write!(f, "{n}"),
            Float(n) => write!(f, "{n}"),
            Char(c) => write!(f, "{c}"),
            Byte(b) => write!(f, "0b{:b}", **b),
        }
    }
}

impl From<String> for Primitive {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}

impl From<&str> for Primitive {
    fn from(value: &str) -> Self {
        if let Ok(str) = Primitive::make_str(value) {
            return str;
        }

        if let Ok(byte) = Primitive::make_byte(value) {
            return byte;
        }

        if let Ok(char) = Primitive::make_char(value) {
            return char;
        }

        if let Ok(int) = Primitive::make_int(value) {
            return int;
        }

        if let Ok(bigint) = Primitive::make_bigint(value) {
            return bigint;
        }

        if let Ok(float) = Primitive::make_float(value) {
            return float;
        }

        if let Ok(bool) = Primitive::make_bool(value) {
            return bool;
        }

        panic!("Invalid constexpr: {value}")
    }
}

pub type BinOp<T> = fn(T, T) -> T;
pub type CheckedBinOp<T> = fn(T, T) -> Option<T>;

pub fn bin_op_from(symbol: char) -> Result<(CheckedBinOp<i32>, CheckedBinOp<i128>, BinOp<f64>)> {
    use std::ops::*;
    Ok(match symbol {
        '+' => (i32::checked_add, i128::checked_add, f64::add),
        '-' => (i32::checked_sub, i128::checked_sub, f64::sub),
        '*' => (i32::checked_mul, i128::checked_mul, f64::mul),
        '/' => (i32::checked_div, i128::checked_div, f64::div),
        '%' => (i32::checked_rem, i128::checked_rem, f64::rem),
        _ => bail!("unknown binary operator ({symbol})"),
    })
}

pub fn bin_op_result(
    left: Primitive,
    right: Primitive,
    i32_fn: CheckedBinOp<i32>,
    i128_fn: CheckedBinOp<i128>,
    f_fn: BinOp<f64>,
) -> Result<Primitive> {
    Ok(match (left, right) {
        (Primitive::Float(x), Primitive::Float(y)) => Primitive::Float(buckets::Float(f_fn(*x, *y))),
        (Primitive::Float(x), Primitive::Int(y)) => Primitive::Float(buckets::Float(f_fn(*x, *y as f64))),
        (Primitive::Int(x), Primitive::Float(y)) => Primitive::Float(buckets::Float(f_fn(*x as f64, *y))),
        (Primitive::Int(x), Primitive::Int(y)) => {
            if let Some(result) = i32_fn(*x, *y) {
                Primitive::Int(buckets::Int(result))
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::Int(y)) => {
            if let Some(result) = i128_fn(*x, *y as i128) {
                Primitive::BigInt(buckets::BigInt(result))
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Int(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(*x as i128, *y) {
                Primitive::BigInt(buckets::BigInt(result))
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(*x, *y) {
                Primitive::BigInt(buckets::BigInt(result))
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (x, y) => bail!("cannot perform binary operation on {x}, {y}"),
    })
}

#[cfg(test)]
mod primitives {
    use std::f64::consts::PI;

    use crate::bytecode::variables::buckets;

    use super::Primitive::{self, *};

    #[test]
    fn byte_1() {
        let var = Primitive::from("0b101".to_owned());
        assert_eq!(var, Byte(buckets::Byte(0b101)));
    }

    #[test]
    fn byte_2() {
        let var = Primitive::from("0b11111111".to_owned());
        assert_eq!(var, Byte(buckets::Byte(0b11111111)));
    }

    #[test]
    fn char() {
        let var = Primitive::from("'@'".to_owned());
        assert_eq!(var, Char(buckets::Char('@')));
    }

    #[test]
    fn display() {
        println!("{}", Byte(buckets::Byte(0b101)));
        println!("{}", Int(buckets::Int(5)));
        println!("{}", Float(buckets::Float(PI)));
        println!("{}", Str(buckets::Str("Hello".into())));
        println!("{}", Bool(buckets::Bool(false)));
        println!("{}", Char(buckets::Char('@')));
    }

    #[test]
    fn is_numeric() {
        assert!(Byte(buckets::Byte(0b1000)).is_numeric());
        assert!(Int(buckets::Int(5)).is_numeric());
        assert!(Float(buckets::Float(3.0 / 2.0)).is_numeric());
        assert!(BigInt(buckets::BigInt(2147483648)).is_numeric());
        assert!(!Str(buckets::Str("Hello".into())).is_numeric());
        assert!(!Bool(buckets::Bool(false)).is_numeric());
        assert!(!Char(buckets::Char('@')).is_numeric());
    }

    #[test]
    fn int() {
        assert_eq!(Primitive::from("2147483647"), Primitive::Int(buckets::Int(i32::MAX)));
        assert_eq!(Primitive::from("2147483648"), Primitive::BigInt(buckets::BigInt(2147483648)));
    }
}
