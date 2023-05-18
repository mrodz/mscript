use std::fmt::Display;

use crate::parser::{Parser, Node};

use super::Dependencies;

#[derive(Debug)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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

impl Dependencies for Ident {}

impl Parser {
    pub fn ident(input: Node) -> Ident {
        input.as_str().into()
    }
}
