use std::fmt::{Debug, Display};

use anyhow::{bail, Result};

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Primitive {
    Bool(bool),
    Str(String),
    Int(i32),
    Float(f64),
    Char(char),
    Byte(u8),
    NaN,
}

impl Primitive {
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Float(_) | Self::Byte(_))
    }

    pub fn is_nan(&self) -> bool {
        matches!(self, Self::NaN)
    }

    pub fn is_numeric_coalesce(&self) -> bool {
        use Primitive::*;

        matches!(self, Bool(_) | Int(_) | Float(_) | Char(_) | Byte(_))
    }

    pub fn raw_size(&self) -> usize {
        use Primitive::*;
        match self {
            Bool(_) => std::mem::size_of::<bool>(),
            Str(str) => str.len(),
            Int(_) => std::mem::size_of::<i32>(),
            Float(_) => std::mem::size_of::<f64>(),
            Char(_) => std::mem::size_of::<char>(),
            Byte(_) => 1,
            NaN => 0,
        }
    }

    pub fn make_str(string: &String) -> Result<Self> {
        let bytes = string.as_bytes();
        if let (Some(b'\"'), Some(b'\"')) = (bytes.get(0), bytes.get(string.len() - 1)) {
            return Ok(Primitive::Str(parse_string(&string).unwrap()));
        }

        bail!("not a Str")
    }

    pub fn make_byte(string: &String) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() >= 3 && &bytes[..2] == [b'0', b'b'] {
            let byte =
                u8::from_str_radix(&string[2..], 2).expect("malformed byte. format: (0b10101010)");

            return Ok(Primitive::Byte(byte));
        }

        bail!("not a Byte")
    }

    pub fn make_char(string: &String) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() == 3 && bytes[0] == b'\'' && bytes[2] == b'\'' {
            return Ok(Primitive::Char(bytes[1].into()));
        }

        bail!("not a Char")
    }

    pub fn make_float(string: &String) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = f64::from_str(&string) {
            return Ok(Primitive::Float(is_f64));
        }

        bail!("not a Float")
    }

    pub fn make_int(string: &String) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_i64) = i32::from_str(&string) {
            return Ok(Primitive::Int(is_i64));
        }

        bail!("not an Int")
    }

    pub fn make_bool(string: &String) -> Result<Self> {
        match string.as_str() {
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
            Float(n) => write!(f, "{n}"),
            Char(c) => write!(f, "{c}"),
            Byte(b) => write!(f, "0b{b:b}"),
            NaN => write!(f, "NaN"),
        }
    }
}

impl From<String> for Primitive {
    fn from(value: String) -> Self {
        Self::from(&value)
    }
}

impl From<&String> for Primitive {
    fn from(value: &String) -> Self {
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

pub fn bin_op_from(symbol: char) -> Result<(fn(i32, i32) -> Option<i32>, BinOp<f64>)> {
    use std::ops::*;
    Ok(match symbol {
        '+' => (i32::checked_add, f64::add),
        '-' => (i32::checked_sub, f64::sub),
        '*' => (i32::checked_mul, f64::mul),
        '/' => (i32::checked_div, f64::div),
        '%' => (i32::checked_rem, f64::rem),
        _ => bail!("unknown binary operator ({symbol})"),
    })
}

pub fn bin_op_result(
    left: Primitive,
    right: Primitive,
    i_fn: fn(i32, i32) -> Option<i32>,
    f_fn: BinOp<f64>,
) -> Result<Primitive> {
    Ok(match (left, right) {
        (Primitive::Float(x), Primitive::Float(y)) => Primitive::Float(f_fn(x, y)),
        (Primitive::Float(x), Primitive::Int(y)) => Primitive::Float(f_fn(x, y as f64)),
        (Primitive::Int(x), Primitive::Float(y)) => Primitive::Float(f_fn(x as f64, y)),
        (Primitive::Int(x), Primitive::Int(y)) => {
            if let Some(result) = i_fn(x, y) {
                Primitive::Int(result)
            } else {
                bail!("could not perform checked integer operation (maybe an overflow, or / by 0)")
            }
        }
        _ => Primitive::NaN,
    })
}

pub fn parse_string(string: &String) -> Result<String> {
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
        assert_eq!(parse_string(&"\"hello\"".into()).unwrap(), "hello");
    }

    #[test]
    fn pre_quot_spaces() {
        assert_eq!(parse_string(&"      \"hello\"".into()).unwrap(), "hello");
    }

    #[test]
    fn post_quot_spaces() {
        assert_eq!(
            parse_string(&"\"hello\"           ".into()).unwrap(),
            "hello"
        );
    }

    #[test]
    fn escaped_quot() {
        assert_eq!(
            parse_string(&"\"Hello \\\"World\\\"\"".into()).unwrap(),
            "Hello \"World\""
        );
    }

    #[test]
    #[should_panic(expected = "EOL")]
    fn eol() {
        parse_string(&"\"Hello".into()).unwrap();
    }

    #[test]
    #[should_panic(expected = "Strings must be surrounded by \"")]
    fn no_start_quot() {
        dbg!(parse_string(&"Hello\"".into()).unwrap());
    }

    #[test]
    fn extra_text() {
        assert_eq!(parse_string(&"\"Hello\" world".into()).unwrap(), "Hello");
    }

    #[test]
    fn escape_sequences() {
        assert_eq!(
            parse_string(&"\"\\n\\r\\t\\\\\\\"\"".into()).unwrap(),
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
        assert!(!Str("Hello".into()).is_numeric());
        assert!(!Bool(false).is_numeric());
        assert!(!Char('@').is_numeric());
    }
}
