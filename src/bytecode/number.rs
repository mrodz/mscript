use std::{fmt::Display, ops::Add};

#[derive(PartialEq, PartialOrd, Debug)]
enum Raw {
    F_32(f32),
    F_64(f64),
    I_32(i32),
    I_64(i64),
    I_128(i128),
}

struct Number {
    raw: Raw,
}

impl From<f32> for Number {
    fn from(value: f32) -> Self {
        Self {
            raw: Raw::F_32(value),
        }
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self {
            raw: Raw::F_64(value),
        }
    }
}

impl From<i32> for Number {
    fn from(value: i32) -> Self {
        Self {
            raw: Raw::I_32(value),
        }
    }
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self {
            raw: Raw::I_64(value),
        }
    }
}

impl From<i128> for Number {
    fn from(value: i128) -> Self {
        Self {
            raw: Raw::I_128(value),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::Raw::*;
        match self.raw {
            F_32(x) => write!(f, "{x}"),
            F_64(x) => write!(f, "{x}"),
            I_32(x) => write!(f, "{x}"),
            I_64(x) => write!(f, "{x}"),
            I_128(x) => write!(f, "{x}"),
        }
    }
}
