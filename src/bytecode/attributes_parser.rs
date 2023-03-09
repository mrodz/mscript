use anyhow::{bail, Result};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Attributes {
    map: HashMap<String, String>,
}

impl Attributes {
    fn attr(&self, name: &String) -> Option<&String> {
        self.map.get(name)
    }
}

pub fn parse_attributes(attribute_str: &String) -> Result<Attributes> {
    static ESCAPING: u8 = 0b100;
    static IN_QUOT: u8 = 0b010;
    static AT_LEFT: u8 = 0b001;

    let mut chars = attribute_str.chars();
    let Some('#') = chars.next() else {
		bail!("attributes must start with '#'.")
	};

    let Some('[') = chars.next() else {
		bail!("attribute syntax: #[...]")
	};

    let mut flags = 0b001;

    let mut map: HashMap<String, String> = HashMap::new();

    let mut left = String::new();

    let mut buffer = String::new();

    #[inline]
    fn is(flags: u8, flag: u8) -> bool {
        flags & flag == flag
    }

    while let Some(c) = chars.next() {
        if is(flags, ESCAPING) {
            flags ^= ESCAPING;
			match c {
				'n' => buffer.push('\n'),
				't' => buffer.push('\t'),
				c => buffer.push(c)
			}
            continue;
        } else if c == ']' {
            if !is(flags, AT_LEFT) {
                map.insert(left.clone(), buffer.clone());
            } else {
                map.insert(buffer.clone(), String::new());
            }
            break;
        }

        if c == '"' {
            flags ^= IN_QUOT;
            continue;
        }

        if is(flags, IN_QUOT) {
            if c == '\\' {
                flags ^= ESCAPING;
                continue;
            }
            buffer.push(c);
            continue;
        }

        if c.is_whitespace() {
            continue;
        }

        if c == '=' {
            if !is(flags, AT_LEFT) {
                bail!("duplicate '='")
            }

            if buffer.len() == 0 {
                bail!("missing key")
            }

            left = buffer.clone();
            buffer.clear();
            flags ^= AT_LEFT;
            continue;
        }

        if c == ',' {
            if !is(flags, AT_LEFT) {
                map.insert(left.clone(), buffer.clone());
            } else {
                map.insert(buffer.clone(), String::new());
            }
            buffer.clear();
            left.clear();

            flags = AT_LEFT;

            continue;
        }

        buffer.push(c)
    }

    Ok(Attributes { map })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() -> Result<()> {
        let info = parse_attributes(&"#[hello=1, bye=2]".into()).unwrap();
        assert_eq!(info.attr(&"hello".into()).unwrap(), "1");
        assert_eq!(info.attr(&"bye".into()).unwrap(), "2");

        Ok(())
    }

    #[test]
    fn strings() -> Result<()> {
        let info = parse_attributes(&"#[hello=\"Lorem Ipsum\", bye=2]".into()).unwrap();
        debug_assert_eq!(info.attr(&"hello".into()).unwrap(), "Lorem Ipsum");
        debug_assert_eq!(info.attr(&"bye".into()).unwrap(), "2");

        Ok(())
    }

    #[test]
    fn escape_sequences() -> Result<()> {
        let info =
            parse_attributes(&"#[hello=\"Lorem\\\"Ipsum\", bye=\"\\\n\", tab=\"\\t\"]".into())
                .unwrap();
        debug_assert_eq!(info.attr(&"hello".into()).unwrap(), "Lorem\"Ipsum");
        debug_assert_eq!(info.attr(&"bye".into()).unwrap(), "\n");
        debug_assert_eq!(info.attr(&"tab".into()).unwrap(), "\t");

        Ok(())
    }

    #[test]
    fn strings_as_keys_and_values() -> Result<()> {
        let info = parse_attributes(&"#[\"this has a space\"=-3, \"this key has an escaped \\\" in it\"=\"str mapped to a\\tstr\"]".into()).unwrap();
        debug_assert_eq!(info.attr(&"this has a space".into()).unwrap(), "-3");
        debug_assert_eq!(
            info.attr(&"this key has an escaped \" in it".into())
                .unwrap(),
            "str mapped to a\tstr"
        );

        Ok(())
    }

    #[test]
    #[should_panic(expected = "start with '#'")]
    fn malformed_start() {
        parse_attributes(&"[hi=\"world\"]".into()).unwrap();
    }

    #[test]
    #[should_panic(expected = "missing key")]
    fn no_key() {
        parse_attributes(&"#[=\"world\"]".into()).unwrap();
    }

    #[test]
    #[should_panic(expected = "duplicate '='")]
    fn too_many_equal_signs_1() {
        parse_attributes(&"#[hi==\"world\"]".into()).unwrap();
    }

    #[test]
    #[should_panic(expected = "duplicate '='")]
    fn too_many_equal_signs_2() {
        parse_attributes(&"#[hi==============================\"world\"]".into()).unwrap();
    }

    #[test]
    fn no_equal_sign() {
        parse_attributes(&"#[huhhhh]".into()).unwrap();
    }

    #[test]
    fn no_equal_sign_and_attr() {
        let info = parse_attributes(&"#[huhhhh, b=5]".into()).unwrap();
        debug_assert_eq!(info.attr(&"huhhhh".into()).unwrap(), "");
        debug_assert_eq!(info.attr(&"b".into()).unwrap(), "5");
    }

    #[test]
    fn squashed_no_equal_sign() {
        let info = parse_attributes(&"#[a=c, \"this has spaces\\nand\\nis an attr\", b=5]".into())
            .unwrap();
        debug_assert_eq!(info.attr(&"a".into()).unwrap(), "c");
        debug_assert_eq!(
            info.attr(&"this has spaces\nand\nis an attr".into())
                .unwrap(),
            ""
        );
        debug_assert_eq!(info.attr(&"b".into()).unwrap(), "5");
    }

    #[test]
    fn key_and_no_value() {
        parse_attributes(&"#[a=,\"this has spaces\\nand\\nis an attr\", b=5]".into()).unwrap();
    }
}
