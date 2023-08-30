//! Implementation of binary operations over primitive values.

mod add;
mod div;
mod mul;
mod ord;
mod rem;
mod sub;

/// Common math operation checks.
///
/// | Input x | Input y | Output |
/// | ------- | ------- | ------ |
/// | Int     | Int     | Int    |
/// | Int     | Float   | Float  |
/// | Int     | BigInt  | BigInt |
/// | Float   | Float   | Float  |
/// | Float   | Int     | Float  |
/// | Float   | BigInt  | Float  |
/// | BigInt  | BigInt  | BigInt |
/// | BigInt  | Int     | BigInt |
/// | BigInt  | Float   | Float  |
/// | Byte    | Byte    | Byte   |
#[macro_export]
macro_rules! apply_math_bin_op_if_applicable {
    ($lhs:ident $symbol:tt $rhs:ident) => {{
        #[allow(unused_imports)]
        use $crate::variables::Primitive::{self, *};
        use $crate::*;

        match ($lhs, $rhs) {
            (Int(x), Int(y)) => Some(int!(x $symbol y)),
            (Int(x), Float(y)) => Some(float!(*x as f64 $symbol y)),
            (Int(x), BigInt(y)) => Some(bigint!(*x as i128 $symbol y)),
            (Int(x), Byte(y)) => Some(int!(*x $symbol *y as i32)),

            (Float(x), Float(y)) => Some(float!(x $symbol y)),
            (Float(x), Int(y)) => Some(float!(x $symbol *y as f64)),
            (Float(x), BigInt(y)) => Some(float!(x $symbol *y as f64)),
            (Float(x), Byte(y)) => Some(float!(x $symbol *y as f64)),

            (BigInt(x), BigInt(y)) => Some(bigint!(x $symbol y)),
            (BigInt(x), Int(y)) => Some(bigint!(x $symbol *y as i128)),
            (BigInt(x), Float(y)) => Some(float!(*x as f64 $symbol y)),
            (BigInt(x), Byte(y)) => Some(bigint!(x $symbol *y as i128 )),
            (Byte(x), Byte(y)) => Some(byte!(x $symbol y)),
            (Byte(x), Int(y)) => Some(int!(*x as i32 $symbol *y)),
            (Byte(x), BigInt(y)) => Some(bigint!(*x as i128 $symbol *y)),
            (Byte(x), Float(y)) => Some(float!(*x as f64 $symbol *y)),

            _ => None
        }
    }}
}

/// Standard casts for boolean comparisons.
#[macro_export]
macro_rules! apply_bool_bin_op_if_applicable {
    ($lhs:ident $symbol:tt $rhs:ident) => {{
        #[allow(unused_imports)]
        use $crate::variables::Primitive::{self, *};

        match ($lhs, $rhs) {
            (Int(x), Int(y)) => x $symbol y,
            (Int(x), Float(y)) => (*x as f64) $symbol *y,
            (Int(x), BigInt(y)) => (*x as i128) $symbol *y,
            (Int(x), Byte(y)) => *x $symbol (*y as i32),

            (Float(x), Float(y)) => x $symbol y,
            (Float(x), Int(y)) => *x $symbol (*y as f64),
            (Float(x), BigInt(y)) => *x $symbol (*y as f64),
            (Float(x), Byte(y)) => *x $symbol (*y as f64),

            (BigInt(x), BigInt(y)) => x $symbol y,
            (BigInt(x), Int(y)) => *x $symbol (*y as i128),
            (BigInt(x), Float(y)) => (*x as f64) $symbol *y,
            (BigInt(x), Byte(y)) => *x $symbol (*y as i128),

            (Byte(x), Byte(y)) => x $symbol y,
            (Byte(x), Int(y)) => (*x as i32) $symbol *y,
            (Byte(x), BigInt(y)) => (*x as i128) $symbol *y,
            (Byte(x), Float(y)) => (*x as f64) $symbol *y,
            _ => panic!("boolean comparison on a non-boolean")
        }
    }}
}
