use crate::parser::{Parser, Node};

#[derive(Debug)]
pub struct Ident(String);

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl Parser {
    pub fn ident(input: Node) -> Ident {
        input.as_str().into()
    }
}
