use crate::{bigint, bool, byte, char, float, int, string, vector};
use anyhow::{bail, Result};
use std::fmt::{Debug, Display};
// use crate::bytecode::variables::vector::Vector;
// pub struct Vector(Vec<crate::bytecode::variables::Primitive>);

macro_rules! primitive {
    ($($variant:ident($type:ty)),+ $(,)?) => {
        pub mod buckets {
            $(
                #[derive(PartialEq, PartialOrd, Clone)]
                pub struct $variant(pub(crate) $type);

                impl std::fmt::Debug for $variant {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                        write!(f, "{:?}", self.0)
                    }
                }

                impl std::fmt::Display for $variant {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
                        write!(f, "{:?}", self.0)
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

        #[derive(Debug, Eq, PartialEq, Clone)]
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
    Bool(bool),
    Str(String),
    Int(i32),
    BigInt(i128),
    Float(f64),
    Char(char),
    Byte(u8),
    Function(crate::bytecode::function::PrimitiveFunction),
    Vector(Vec<crate::bytecode::variables::Primitive>),
    Object(crate::bytecode::variables::Object),
}

#[derive(Clone, PartialEq)]
pub struct Variable {
    pub data: Primitive,
    pub ty: Type,
}

impl From<Primitive> for Variable {
    fn from(value: Primitive) -> Self {
        Self {
            ty: value.ty(),
            data: value,
        }
    }
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

impl Primitive {
    pub fn equals(&self, rhs: &Self) -> Result<bool> {
        macro_rules! impl_eq {
            (each $($lhs_and_rhs:ident),+ $(,)? with itself) => {
                match (self, rhs) {
                    $(
                        (Primitive::$lhs_and_rhs(x), Primitive::$lhs_and_rhs(y)) => return Ok(x == y),
                    )*
                    _ => bail!("cannot compare {:?} with {:?}", self.ty(), rhs.ty())

                }
            };
            ($lhs:ident with $($rhs:ident),+ $(,)?) => {
                if let Primitive::$lhs(x) = self {
                    let result = match rhs {
                        Primitive::$lhs(y) => Ok(x == y),
                        $(
                            Primitive::$rhs(y) => Ok(x == y),
                        )*
                        _ => bail!("cannot compare {:?} with {:?}", self.ty(), rhs.ty())
                    };

                    return result;
                }
            };
        }

        impl_eq!(Int with Float, BigInt, Byte);
        impl_eq!(Float with Int, BigInt, Byte);
        impl_eq!(BigInt with Float, Int, Byte);
        impl_eq!(Byte with Float, BigInt, Int);

        impl_eq!(each Str, Char, Bool with itself);
    }

    pub fn is_numeric(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Int | Float | Byte | BigInt)
    }

    pub fn is_numeric_coalesce(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Bool | Int | Float | Char | Byte | BigInt)
    }

    pub fn make_str(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if let (Some(b'\"'), Some(b'\"')) = (bytes.first(), bytes.last()) {
            let sliced = &string[1..string.len() - 1];
            return Ok(string!(raw sliced));
        }

        bail!("not a Str")
    }

    pub fn make_byte(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() >= 3 && &bytes[..2] == [b'0', b'b'] {
            let byte =
                u8::from_str_radix(&string[2..], 2).expect("malformed byte. format: (0b10101010)");

            return Ok(byte!(byte));
        }

        bail!("not a Byte")
    }

    pub fn make_char(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() == 3 && bytes[0] == b'\'' && bytes[2] == b'\'' {
            return Ok(char!(bytes[1].into()));
        }

        bail!("not a Char")
    }

    pub fn make_float(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = f64::from_str(&string) {
            return Ok(float!(is_f64));
        }

        bail!("not a Float")
    }

    pub fn make_int(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_i64) = i32::from_str(&string) {
            return Ok(int!(is_i64));
        }

        bail!("not an Int")
    }

    pub fn make_bigint(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = i128::from_str(&string) {
            return Ok(bigint!(is_f64));
        }

        bail!("not a BigInt")
    }

    pub fn make_bool(string: &str) -> Result<Self> {
        match string {
            "true" => Ok(bool!(true)),
            "false" => Ok(bool!(false)),
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
            Function(fun) => write!(f, "{fun}"),
            Vector(l) => write!(f, "{l}"),
            Object(o) => write!(f, "{o}"),
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
        (Primitive::Float(x), Primitive::Float(y)) => float!(f_fn(*x, *y)),
        (Primitive::Float(x), Primitive::Int(y)) => float!(f_fn(*x, *y as f64)),
        (Primitive::Int(x), Primitive::Float(y)) => float!(f_fn(*x as f64, *y)),
        (Primitive::Int(x), Primitive::Int(y)) => {
            if let Some(result) = i32_fn(*x, *y) {
                int!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::Int(y)) => {
            if let Some(result) = i128_fn(*x, *y as i128) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Int(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(*x as i128, *y) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(*x, *y) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Str(x), y) => {
            let mut x = (*x).clone();

            if let Primitive::Str(y) = y {
                x.push_str(&*y); // slight performance benefit
            } else {
                x.push_str(&y.to_string());
            }

            string!(x)
        }
        (Primitive::Vector(x), Primitive::Vector(y)) => {
            // if `adding` vectors, the user probably wants a new copy to operate on.
            // to extend a vector, use a `vec_op` instruction.

            let mut x_cloned = (*x).clone();
            let mut y_cloned = (*y).clone();

            x_cloned.append(&mut y_cloned);

            vector!(raw x_cloned)
        }
        (x, y) => bail!("cannot perform binary operation on {x}, {y}"),
    })
}

#[cfg(test)]
mod primitives {
    use super::Primitive;
    use crate::{bigint, bool, byte, bytecode::variables::buckets, char, float, int, string};
    use std::f64::consts::PI;

    #[test]
    fn byte_1() {
        let var = Primitive::from("0b101".to_owned());
        assert_eq!(var, byte!(0b101));
    }

    #[test]
    fn byte_2() {
        let var = Primitive::from("0b11111111".to_owned());
        assert_eq!(var, byte!(0b11111111));
    }

    #[test]
    fn char() {
        let var = Primitive::from("'@'".to_owned());
        assert_eq!(var, char!('@'));
    }

    #[test]
    fn display() {
        println!("{}", byte!(0b101));
        println!("{}", int!(5));
        println!("{}", float!(PI));
        println!("{}", string!(raw "Hello"));
        println!("{}", bool!(false));
        println!("{}", char!('@'));
    }

    #[test]
    fn is_numeric() {
        assert!(byte!(0b1000).is_numeric());
        assert!(int!(5).is_numeric());
        assert!(float!(3.0 / 2.0).is_numeric());
        assert!(bigint!(2147483648).is_numeric());
        assert!(!string!(raw "Hello").is_numeric());
        assert!(!bool!(true).is_numeric());
        assert!(!char!('@').is_numeric());
    }

    #[test]
    fn int() {
        assert_eq!(Primitive::from("2147483647"), int!(i32::MAX));
        assert_eq!(Primitive::from("2147483648"), bigint!(2147483648));
    }
}
