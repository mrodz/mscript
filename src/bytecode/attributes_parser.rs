use anyhow::{bail, Result};
use std::{borrow::Cow, collections::HashMap, fmt::Display, ops::Index};

#[derive(Debug)]
pub struct Attributes {
    map: HashMap<String, Option<String>>,
}

impl Index<String> for Attributes {
    type Output = Option<String>;
    fn index(&self, index: String) -> &Self::Output {
        self.attr(&index).unwrap()
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        let mut iter = self.map.iter();

        fn quot_if_needed(text: &String) -> Cow<String> {
            if !text.contains(' ') {
                return Cow::Borrowed(text);
            }

            Cow::Owned("\"".to_owned() + text + "\"")
        }

        fn format_pair(key: &String, value: &Option<String>) -> String {
            let (value_len, quot_size) = if let Some(v) = value {
                (v.len(), 4)
            } else {
                (0, 2)
            };

            let len = key.len() + value_len + quot_size;
            let mut result = String::with_capacity(len);
            result.push_str(&quot_if_needed(key));

            if let Some(value) = value {
                result.push('=');
                result.push_str(&quot_if_needed(value));
            }

            result
        }

        'prepare_string: {
            let Some((key, value)) = iter.next() else {
                break 'prepare_string;
            };

            str.push_str(&format_pair(key, value));

            for (key, val) in iter {
                str.push_str(", ");
                str.push_str(&format_pair(key, val));
            }
        }

        write!(f, "#[{str}]")
    }
}

impl Attributes {
    fn attr(&self, name: &String) -> Option<&Option<String>> {
        self.map.get(name)
    }
}

pub fn parse_attributes(attribute_str: &String) -> Result<Attributes> {
    const WHITESPACE: u8 = 0b1000;
    const ESCAPING: u8 = 0b0100;
    const IN_QUOT: u8 = 0b0010;
    const AT_LEFT: u8 = 0b0001;

    let mut chars = attribute_str.chars();

    match (chars.next(), chars.next()) {
        (Some('#'), Some('[')) => {}
        _ => bail!("attribute syntax: #[key1=value, key2=\"value\", ...]"),
    }

    let mut flags = AT_LEFT;

    let mut map: HashMap<String, Option<String>> = HashMap::new();

    let mut left = String::new();
    let mut buffer = String::new();

    #[inline]
    fn is(flags: u8, flag: u8) -> bool {
        flags & flag == flag
    }

    #[inline]
    fn flush(flags: u8, left: &mut String, right: &mut String) -> Result<(String, Option<String>)> {
        if is(flags, AT_LEFT) && right.len() == 0 {
            bail!("extra ','")
        }

        let result = if !is(flags, AT_LEFT) {
            (left.clone(), Some(right.clone()))
        } else {
            (right.clone(), None)
        };
        left.clear();
        right.clear();

        Ok(result)
    }

    while let Some(c) = chars.next() {
        if is(flags, ESCAPING) {
            flags ^= ESCAPING;
            buffer.push(match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                c => c,
            });
            continue;
        } else if c == ']' {
            let (key, value) = flush(flags, &mut left, &mut buffer)?;
            map.insert(key, value);
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
            flags |= WHITESPACE;
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
            let (key, value) = flush(flags, &mut left, &mut buffer)?;
            map.insert(key, value);
            buffer.clear();
            left.clear();

            flags = AT_LEFT;

            continue;
        }

        buffer.push(c)
    }

    if buffer.len() != 0 {
        bail!("attribute syntax: #[key1=value, key2=\"value\", ...]")
    }

    Ok(Attributes { map })
}

#[macro_export]
macro_rules! assert_key_val {
    ($map:ident[$key:tt] == $val:tt) => {
        let val = $map.attr(&$key.into()).unwrap();
        if let Some(val) = val {
            assert_eq!(val, $val);
        }
    };
    ($map:ident[$key:tt] exists) => {
        assert!($map.attr(&$key.into()).is_some());
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic() -> Result<()> {
        let info = parse_attributes(&"#[hello=1, bye=2]".into()).unwrap();

        assert_key_val!(info["hello"] == "1");
        assert_key_val!(info["bye"] == "2");

        Ok(())
    }

    #[test]
    fn strings() -> Result<()> {
        let info = parse_attributes(&"#[hello=\"Lorem Ipsum\", bye=2]".into()).unwrap();

        assert_key_val!(info["hello"] == "Lorem Ipsum");
        assert_key_val!(info["bye"] == "2");

        Ok(())
    }

    #[test]
    fn escape_sequences() -> Result<()> {
        let info = parse_attributes(
            &"#[hello=\"Lorem\\\"Ipsum\\r\\n\", bye=\"\\\n\", tab=\"\\t\"]".into(),
        )
        .unwrap();

        assert_key_val!(info["hello"] == "Lorem\"Ipsum\r\n");
        assert_key_val!(info["bye"] == "\n");
        assert_key_val!(info["tab"] == "\t");

        Ok(())
    }

    #[test]
    fn strings_as_keys_and_values() -> Result<()> {
        let info = parse_attributes(&"#[\"this has a space\"=-3, \"this key has an escaped \\\" in it\"=\"str mapped to a\\tstr\"]".into()).unwrap();

        assert_key_val!(info["this has a space"] == "-3");
        assert_key_val!(info["this key has an escaped \" in it"] == "str mapped to a\tstr");

        Ok(())
    }

    #[test]
    #[should_panic(expected = "attribute syntax")]
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
        parse_attributes(&"#[hi=========\"world\"]".into()).unwrap();
    }

    #[test]
    fn no_equal_sign() {
        parse_attributes(&"#[huhhhh]".into()).unwrap();
    }

    #[test]
    fn no_equal_sign_and_attr() {
        let info = parse_attributes(&"#[huhhhh, b=5]".into()).unwrap();

        assert_key_val!(info["huhhhh"] exists);
        assert_key_val!(info["b"] == "5");
    }

    #[test]
    fn squashed_no_equal_sign() {
        let info = parse_attributes(&"#[a=c, \"this has spaces\\nand\\nis an attr\", b=5]".into())
            .unwrap();

        assert_key_val!(info["a"] == "c");
        assert_key_val!(info["this has spaces\nand\nis an attr"] exists);
        assert_key_val!(info["b"] == "5");
    }

    #[test]
    fn key_and_no_value() {
        parse_attributes(&"#[a=,\"this has spaces\\nand\\nis an attr\", b=5]".into()).unwrap();
    }

    #[test]
    #[should_panic(expected = "extra ','")]
    fn multiple_commas() {
        let info = parse_attributes(&"#[a=c,, \"this has spaces\\nand\\nis an attr\", b=5]".into())
            .unwrap();
        dbg!(info);
    }

    #[test]
    #[should_panic(expected = "attribute syntax")]
    fn incomplete() {
        let info = parse_attributes(&"#[this".into()).unwrap();
        dbg!(info);
    }

    #[test]
    fn leading_whitespaces() {
        let info = parse_attributes(&"#[   this=1,    that=2]".into()).unwrap();
        dbg!(info);
    }

    #[test]
    fn trailing_whitespace() {
        let info = parse_attributes(&"#[this  = hello   , that      = 2   ]".into()).unwrap();

        assert_key_val!(info["this"] == "hello");
        assert_key_val!(info["that"] == "2");

        dbg!(info);
    }
}
