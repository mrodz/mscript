//! The interpreter's primitive datatypes.
//! Every "value" in the interpreter is a primitive.

use crate::{bigint, bool, byte, float, int, string};
use anyhow::{bail, Result};
use std::fmt::Display;

/// This macro allows easy recursion over variants.
macro_rules! primitive {
    ($($variant:ident($type:ty)),+ $(,)?) => {
        /// Every possible form of variable data used in the interpreter. 
        #[derive(PartialEq, Debug, Clone)]
        pub enum Primitive {
            $(
                $variant($type),
            )*
        }

        #[derive(Debug, Eq, PartialEq, Clone)]
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
    Bool(bool),
    Str(String),
    Int(i32),
    BigInt(i128),
    Float(f64),
    Byte(u8),
    Function(std::rc::Rc<crate::function::PrimitiveFunction>),
    Vector(std::rc::Rc<Vec<crate::variables::Primitive>>),
    Object(std::rc::Rc<crate::variables::Object>),
}

impl Primitive {
    /// Compare two [`Primitive`] values.
    /// 
    /// # Errors
    /// Will error if the primitive values cannot be compared.
    pub fn equals(&self, rhs: &Self) -> Result<bool> {
        /// Form:
        /// ## Allows VariantBase == Variant1, VariantBase == Variant2, ...
        /// ```ignore
        /// impl_eq!(VariantBase with Variant1(r=CoerseTo), Variant2(r=CoerseTo), Variant3(r=CoerseTo));
        /// ```
        /// 
        /// ## Allows Variant4 == Variant4, Variant5 == Variant5        
        /// ```ignore
        /// impl_eq!(each Variant4, Variant5 with itself);
        /// ```
        macro_rules! impl_eq {
            (each $($lhs_and_rhs:ident),+ $(,)? with itself) => {
                match (self, rhs) {
                    $(
                        (Primitive::$lhs_and_rhs(x), Primitive::$lhs_and_rhs(y)) => return Ok(x == y),
                    )*
                    _ => bail!("cannot compare {:?} with {:?}", self.ty(), rhs.ty())

                }
            };
            ($lhs:ident with $($rhs:ident(r=$type:ty)),+ $(,)?) => {
                if let Primitive::$lhs(x) = self {
                    let result = match rhs {
                        Primitive::$lhs(y) => Ok(x == y),
                        $(
                            Primitive::$rhs(y) => Ok(*x as $type == *y as $type),
                        )*
                        _ => bail!("cannot compare {:?} with {:?}", self.ty(), rhs.ty())
                    };

                    return result;
                }
            };
        }

        impl_eq!(Int with Float(r=f64), BigInt(r=i128), Byte(r=i32));
        impl_eq!(Float with Int(r=f64), BigInt(r=f64), Byte(r=f64));
        impl_eq!(BigInt with Float(r=f64), Int(r=i128), Byte(r=i128));
        impl_eq!(Byte with Float(r=f64), BigInt(r=i128), Int(r=i32));

        impl_eq!(each Str, Bool with itself);
    }

    /// Returns whether this primitive is numeric.
    pub fn is_numeric(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Int | Float | Byte | BigInt)
    }

    /// Returns whether this primitive is a numeric coalesce.
    pub fn is_numeric_coalesce(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Bool | Int | Float | Byte | BigInt)
    }

    /// Parse a string slice to a bytecode [`Primitive::Str`] primitive.
    pub fn make_str(string: &str) -> Result<Self> {
        Ok(string!(raw string))
    }

    /// Parse a string slice to a bytecode [`Primitive::Byte`] primitive.
    pub fn make_byte(string: &str) -> Result<Self> {
        let bytes = string.as_bytes();

        if string.len() >= 3 && bytes[..2] == [b'0', b'b'] {
            let byte =
                u8::from_str_radix(&string[2..], 2).expect("malformed byte. format: (0b10101010)");

            return Ok(byte!(byte));
        }

        bail!("not a Byte")
    }

    /// Parse a string slice to a bytecode [`Primitive::Float`] primitive.
    pub fn make_float(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = f64::from_str(string) {
            return Ok(float!(is_f64));
        }

        bail!("not a Float")
    }

    /// Parse a string slice to a bytecode [`Primitive::Int`] primitive.
    pub fn make_int(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_i64) = i32::from_str(string) {
            return Ok(int!(is_i64));
        }

        bail!("not an Int")
    }

    /// Parse a string slice to a bytecode [`Primitive::BigInt`] primitive.
    pub fn make_bigint(string: &str) -> Result<Self> {
        use std::str::FromStr;

        if let Ok(is_f64) = i128::from_str(string) {
            return Ok(bigint!(is_f64));
        }

        bail!("not a BigInt")
    }

    /// Parse a string slice to a bytecode [`Primitive::Bool`] primitive.
    pub fn make_bool(string: &str) -> Result<Self> {
        match string {
            "true" => Ok(bool!(true)),
            "false" => Ok(bool!(false)),
            _ => bail!("not a Bool"),
        }
    }

    /// Attempt to negate a bytecode primitive.
    /// 
    /// # Errors
    /// Will error if the primitive is not a number.
    pub fn negate(&mut self) -> Result<()> {
        match self {
            Primitive::BigInt(x) => *x = -*x,
            Primitive::Int(x) => *x = -*x,
            Primitive::Float(x) => *x = -*x,
            ty => bail!("cannot negate {ty}"),
        }

        Ok(())
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Primitive::*;

        match self {
            Bool(b) => write!(f, "{}", *b),
            Str(s) => write!(f, "{s}"),
            Int(n) => write!(f, "{n}"),
            BigInt(n) => write!(f, "{n}"),
            Float(n) => write!(f, "{n}"),
            Byte(b) => write!(f, "0b{:b}", *b),
            Function(fun) => write!(f, "{fun}"),
            Vector(l) => write!(f, "{l:?}"),
            Object(o) => write!(f, "{o}"),
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
        if let Ok(byte) = Primitive::make_byte(value) {
            return byte;
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

        if let Ok(str) = Primitive::make_str(value) {
            return str;
        }

        panic!("Invalid constexpr: {value}")
    }
}

#[cfg(test)]
mod tests {
    use crate::{bigint, bool, byte, float, int, string};
    use super::Primitive;
    use std::f64::consts::PI;

    #[test]
    fn byte_1() {
        let var = Primitive::from("0b101".to_owned());
        assert_eq!(var, byte!(0b101));
    }

    #[test]
    fn byte_2() {
        let var = Primitive::from("0b11111111".to_owned());
        assert_eq!(var, byte!(0b11111111));
    }

    #[test]
    fn display() {
        println!("{}", byte!(0b101));
        println!("{}", int!(5));
        println!("{}", float!(PI));
        println!("{}", string!(raw "Hello"));
        println!("{}", bool!(false));
    }

    #[test]
    fn is_numeric() {
        assert!(byte!(0b1000).is_numeric());
        assert!(int!(5).is_numeric());
        assert!(float!(3.0 / 2.0).is_numeric());
        assert!(bigint!(2147483648).is_numeric());
        assert!(!string!(raw "Hello").is_numeric());
        assert!(!bool!(true).is_numeric());
    }

    #[test]
    fn int() {
        assert_eq!(Primitive::from("2147483647"), int!(i32::MAX));
        assert_eq!(Primitive::from("2147483648"), bigint!(2147483648));
    }
}
