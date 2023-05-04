use crate::{bigint, bool, byte, float, int, string, vector};
use anyhow::{bail, Result};
use std::{fmt::{Debug, Display}};

use super::{Primitive, primitive::Type};

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
            ($lhs:ident with $($rhs:ident(r=$type:ty)),+ $(,)?) => {
                if let Primitive::$lhs(x) = self {
                    let result = match rhs {
                        Primitive::$lhs(y) => Ok(x == y),
                        $(
                            Primitive::$rhs(y) => Ok(*x as $type == *y as $type),
                        )*
                        _ => bail!("cannot compare {:?} with {:?}", self.ty(), rhs.ty())
                    };

                    return result;
                }
            };
        }

        impl_eq!(Int with Float(r=f64), BigInt(r=i128), Byte(r=i32));
        impl_eq!(Float with Int(r=f64), BigInt(r=f64), Byte(r=f64));
        impl_eq!(BigInt with Float(r=f64), Int(r=i128), Byte(r=i128));
        impl_eq!(Byte with Float(r=f64), BigInt(r=i128), Int(r=i32));

        impl_eq!(each Str, Bool with itself);
    }

    pub fn is_numeric(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Int | Float | Byte | BigInt)
    }

    pub fn is_numeric_coalesce(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Bool | Int | Float | Byte | BigInt)
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
            // Char(c) => write!(f, "{c}"),
            Byte(b) => write!(f, "0b{:b}", *b),
            Function(fun) => write!(f, "{fun}"),
            Vector(l) => write!(f, "{l:?}"),
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

pub type BinOpFn<T> = fn(T, T) -> T;
pub type CheckedBinOpFn<T> = fn(T, T) -> Option<T>;

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Lte,
    Gt,
    Gte,

}

macro_rules! ez_op {
    ($op_fn:ident) => {
        (i32::$op_fn, i128::$op_fn, f64::$op_fn)
    };
    (int = $op_fn_check:ident, float = $op_fn_float:ident) => {
        (i32::$op_fn_check, i128::$op_fn_check, f64::$op_fn_float)
    };
}

pub fn math_op_from(symbol: char) -> Result<(CheckedBinOpFn<i32>, CheckedBinOpFn<i128>, BinOpFn<f64>)> {
    use std::ops::*;

    Ok(match symbol {
        '+' => ez_op!(int = checked_add, float = add),
        '-' => ez_op!(int = checked_sub, float = sub),
        '*' => ez_op!(int = checked_mul, float = mul),
        '/' => ez_op!(int = checked_div, float = div),
        '%' => ez_op!(int = checked_rem, float = rem),
        _ => bail!("unknown binary operator ({symbol})"),
    })
}

pub fn bin_op_result(
    left: Primitive,
    right: Primitive,
    i32_fn: CheckedBinOpFn<i32>,
    i128_fn: CheckedBinOpFn<i128>,
    f_fn: BinOpFn<f64>,
) -> Result<Primitive> {
    Ok(match (left, right) {
        (Primitive::Float(x), Primitive::Float(y)) => float!(f_fn(x, y)),
        (Primitive::Float(x), Primitive::Int(y)) => float!(f_fn(x, y as f64)),
        (Primitive::Int(x), Primitive::Float(y)) => float!(f_fn(x as f64, y)),
        (Primitive::Int(x), Primitive::Int(y)) => {
            if let Some(result) = i32_fn(x, y) {
                int!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::Int(y)) => {
            if let Some(result) = i128_fn(x, y as i128) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Int(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(x as i128, y) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(x, y) {
                bigint!(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Str(x), y) => {
            let mut x = x.clone();

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

            let mut x_cloned = x.clone();
            let mut y_cloned = y.clone();

            x_cloned.append(&mut y_cloned);

            vector!(raw x_cloned)
        }
        (x, y) => bail!("cannot perform binary operation on {x}, {y}"),
    })
}

#[cfg(test)]
mod primitives {
    use super::Primitive;
    use crate::{bigint, bool, byte, float, int, string};
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
    fn display() {
        println!("{}", byte!(0b101));
        println!("{}", int!(5));
        println!("{}", float!(PI));
        println!("{}", string!(raw "Hello"));
        println!("{}", bool!(false));
    }

    #[test]
    fn is_numeric() {
        assert!(byte!(0b1000).is_numeric());
        assert!(int!(5).is_numeric());
        assert!(float!(3.0 / 2.0).is_numeric());
        assert!(bigint!(2147483648).is_numeric());
        assert!(!string!(raw "Hello").is_numeric());
        assert!(!bool!(true).is_numeric());
    }

    #[test]
    fn int() {
        assert_eq!(Primitive::from("2147483647"), int!(i32::MAX));
        assert_eq!(Primitive::from("2147483648"), bigint!(2147483648));
    }
}
