use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use anyhow::{bail, Result};

#[derive(PartialEq, PartialOrd, Debug)]
pub enum Number {
    F_32(f32),
    F_64(f64),
    I_32(i32),
    I_64(i64),
    I_128(i128),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Number::*;
        match self {
            F_32(x) => write!(f, "{x}"),
            F_64(x) => write!(f, "{x}"),
            I_32(x) => write!(f, "{x}"),
            I_64(x) => write!(f, "{x}"),
            I_128(x) => write!(f, "{x}"),
        }
    }
}

impl Eq for Number {}
impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {}
}

pub enum Type {
    Bool,
    Str,
    Num,
    Char,
    Byte,
}

#[derive(PartialEq, PartialOrd, Eq, Hash, Debug)]

pub enum Primitive {
    Bool(bool),
    Str(String),
    Num(Number),
    Char(char),
    Byte(u8),
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::bytecode::variable::Primitive::*;

        match self {
            Bool(b) => write!(f, "{b}"),
            Str(s) => write!(f, "{s:?}"),
            Num(n) => write!(f, "{n}"),
            Char(c) => write!(f, "{c}"),
            Byte(b) => write!(f, "{b:b}"),
        }
    }
}

impl From<String> for Primitive {
    fn from(value: String) -> Self {
		let bytes = value.as_bytes();
		let len = bytes.len();

        if let (Some(b'\"'), Some(b'\"')) = (bytes.get(0), bytes.get(len - 1)) {
            return Primitive::Str(parse_string(&value).unwrap());
        }

		if len >= 3 && &bytes[..2] == [b'0', b'b'] {
			let byte = u8::from_str_radix(&value[2..], 2)
				.expect("malformed byte. format: (0b10101010)");

			return Primitive::Byte(byte);
		}

		if len == 3 && bytes[0] == b'\'' && bytes[2] == b'\'' {
			return Primitive::Char(bytes[1].into())
		}

        todo!("Primitive::Num, Primitive::Bool")
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
    use std::f32::consts::PI;

    use super::Number::*;
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
        println!("{}", Num(I_32(5)));
        println!("{}", Num(F_32(PI)));
        println!("{}", Str("Hello".into()));
        println!("{}", Bool(false));
        println!("{}", Char('@'));
    }
}
