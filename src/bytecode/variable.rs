use std::fmt::{Debug, Display};

use anyhow::{bail, Result};

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Primitive {
    Bool(bool),
    Str(String),
    Int(i32),
    Float(f64),
    Char(char),
    Byte(u8),
}

impl Primitive {
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
            Byte(b) => write!(f, "{b:b}"),
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

pub fn parse_string(string: &String) -> Result<String> {
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut escaping = false;

    for char in string.chars() {
        if !in_quotes {
            if char == ' ' {
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
        bail!("Found EOL while parsing string")
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
        println!("{}", Int(5));
        println!("{}", Float(PI));
        println!("{}", Str("Hello".into()));
        println!("{}", Bool(false));
        println!("{}", Char('@'));
    }
}
