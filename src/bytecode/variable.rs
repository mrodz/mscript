use std::fmt::{Debug, Display};

use anyhow::{bail, Result};

macro_rules! primitive {
    ($($variant:ident = $type:ty)+) => {
        #[derive(PartialEq, PartialOrd, Debug, Clone)]
        pub enum Primitive {
            $(
                $variant($type),
            )*
        }

        #[derive(Debug)]
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
    Bool = bool
    Str = String
    Int = i32
    BigInt = i128
    Float = f64
    Char = char
    Byte = u8
}

pub struct Variable {
    data: Primitive,
    ty: Type,
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
        if let (Some(b'\"'), Some(b'\"')) = (bytes.get(0), bytes.get(string.len() - 1)) {
            return Ok(Primitive::Str(parse_string(&string).unwrap()));
        }

        bail!("not a Str")
    }

    pub fn make_byte(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() >= 3 && &bytes[..2] == [b'0', b'b'] {
            let byte =
                u8::from_str_radix(&string[2..], 2).expect("malformed byte. format: (0b10101010)");

            return Ok(Primitive::Byte(byte));
        }

        bail!("not a Byte")
    }

    pub fn make_char(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() == 3 && bytes[0] == b'\'' && bytes[2] == b'\'' {
            return Ok(Primitive::Char(bytes[1].into()));
        }

        bail!("not a Char")
    }

    pub fn make_float(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = f64::from_str(&string) {
            return Ok(Primitive::Float(is_f64));
        }

        bail!("not a Float")
    }

    pub fn make_int(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_i64) = i32::from_str(&string) {
            return Ok(Primitive::Int(is_i64));
        }

        bail!("not an Int")
    }

    pub fn make_bigint(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = i128::from_str(&string) {
            return Ok(Primitive::BigInt(is_f64));
        }

        bail!("not a BigInt")
    }

    pub fn make_bool(string: &str) -> Result<Self> {
        match string {
            "true" => Ok(Primitive::Bool(true)),
            "false" => Ok(Primitive::Bool(false)),
            _ => bail!("not a Bool"),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::bytecode::variable::Primitive::*;

        match self {
            Bool(b) => write!(f, "{b}"),
            Str(s) => write!(f, "{s:?}"),
            Int(n) => write!(f, "{n}"),
            BigInt(n) => write!(f, "{n}"),
            Float(n) => write!(f, "{n}"),
            Char(c) => write!(f, "{c}"),
            Byte(b) => write!(f, "0b{b:b}"),
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
        (Primitive::Float(x), Primitive::Float(y)) => Primitive::Float(f_fn(x, y)),
        (Primitive::Float(x), Primitive::Int(y)) => Primitive::Float(f_fn(x, y as f64)),
        (Primitive::Int(x), Primitive::Float(y)) => Primitive::Float(f_fn(x as f64, y)),
        (Primitive::Int(x), Primitive::Int(y)) => {
            if let Some(result) = i32_fn(x, y) {
                Primitive::Int(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::Int(y)) => {
            if let Some(result) = i128_fn(x, y as i128) {
                Primitive::BigInt(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::Int(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(x as i128, y) {
                Primitive::BigInt(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (Primitive::BigInt(x), Primitive::BigInt(y)) => {
            if let Some(result) = i128_fn(x, y) {
                Primitive::BigInt(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        (x, y) => bail!("cannot perform binary operation on {x}, {y}"),
    })
}

pub fn parse_string(string: &str) -> Result<String> {
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes {
            if char.is_ascii_whitespace() {
                continue;
            } else if char != '"' {
                panic!("Strings must be surrounded by \"..\", saw {char}")
            }
        }

        match char {
            '\\' => {
                if escaping {
                    buf.push(char);
                }
                escaping = !escaping;
                continue;
            }
            '"' => {
                if escaping {
                    buf.push(char);
                    escaping = false;
                    continue;
                }

                in_quotes = !in_quotes;

                if !in_quotes {
                    break;
                }

                continue;
            }
            'n' => {
                if escaping {
                    buf.push('\n');
                    escaping = false;
                    continue;
                }
            }
            'r' => {
                if escaping {
                    buf.push('\r');
                    escaping = false;
                    continue;
                }
            }
            't' => {
                if escaping {
                    buf.push('\t');
                    escaping = false;
                    continue;
                }
            }
            _ => {
                if escaping {
                    bail!("unknown escape sequence (\\{char})");
                }
            }
        }

        buf.push(char);
    }

    if in_quotes {
        bail!("found EOL while parsing string")
    } else {
        Ok(buf)
    }
}

#[cfg(test)]
mod string_util {
    use crate::bytecode::variable::parse_string;

    #[test]
    fn basic() {
        assert_eq!(parse_string(&"\"hello\"").unwrap(), "hello");
    }

    #[test]
    fn pre_quot_spaces() {
        assert_eq!(parse_string(&"      \"hello\"").unwrap(), "hello");
    }

    #[test]
    fn post_quot_spaces() {
        assert_eq!(parse_string(&"\"hello\"           ").unwrap(), "hello");
    }

    #[test]
    fn escaped_quot() {
        assert_eq!(
            parse_string(&"\"Hello \\\"World\\\"\"").unwrap(),
            "Hello \"World\""
        );
    }

    #[test]
    #[should_panic(expected = "EOL")]
    fn eol() {
        parse_string(&"\"Hello").unwrap();
    }

    #[test]
    #[should_panic(expected = "Strings must be surrounded by \"")]
    fn no_start_quot() {
        dbg!(parse_string(&"Hello\"").unwrap());
    }

    #[test]
    fn extra_text() {
        assert_eq!(parse_string(&"\"Hello\" world").unwrap(), "Hello");
    }

    #[test]
    fn escape_sequences() {
        assert_eq!(
            parse_string(&"\"\\n\\r\\t\\\\\\\"\"").unwrap(),
            "\n\r\t\\\""
        );
    }
}

#[cfg(test)]
mod primitives {
    use std::f64::consts::PI;

    use super::Primitive::{self, *};

    #[test]
    fn string_1() {
        let var = Primitive::from(r#""Hello\" world""#.to_owned());
        assert_eq!(var, Str("Hello\" world".into()))
    }

    #[test]
    fn byte_1() {
        let var = Primitive::from("0b101".to_owned());
        assert_eq!(var, Byte(0b101));
    }

    #[test]
    fn byte_2() {
        let var = Primitive::from("0b11111111".to_owned());
        assert_eq!(var, Byte(0b11111111));
    }

    #[test]
    fn char() {
        let var = Primitive::from("'@'".to_owned());
        assert_eq!(var, Char('@'));
    }

    #[test]
    fn display() {
        println!("{}", Byte(0b101));
        println!("{}", Int(5));
        println!("{}", Float(PI));
        println!("{}", Str("Hello".into()));
        println!("{}", Bool(false));
        println!("{}", Char('@'));
    }

    #[test]
    fn is_numeric() {
        assert!(Byte(0b1000).is_numeric());
        assert!(Int(5).is_numeric());
        assert!(Float(3.0 / 2.0).is_numeric());
        assert!(BigInt(2147483648).is_numeric());
        assert!(!Str("Hello".into()).is_numeric());
        assert!(!Bool(false).is_numeric());
        assert!(!Char('@').is_numeric());
    }

    #[test]
    fn int() {
        assert_eq!(Primitive::from("2147483647"), Primitive::Int(i32::MAX));
        assert_eq!(Primitive::from("2147483648"), Primitive::BigInt(2147483648));
    }
}
