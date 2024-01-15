//! The interpreter's primitive datatypes.
//! Every "value" in the interpreter is a primitive.

use crate::{
    bigint, bool, byte, float,
    function::BuiltInFunction,
    int,
    stack::{PrimitiveFlagsPair, VariableMapping, PRIMITIVE_MODULE},
    string,
};
use anyhow::{bail, Result};
use std::{
    cell::{RefCell, RefMut, Ref},
    fmt::{Debug, Display},
    rc::Rc, ops::Deref,
};

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
            Nil,
            $(
                $variant,
            )*
        }

        impl Primitive {
            pub fn ty(&self) -> Type {
                match self {
                    Primitive::Optional(None) => Type::Nil,
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

#[derive(Debug, Clone)]
pub enum HeapPrimitive {
    ArrayPtr(Rc<RefCell<Vec<Primitive>>>, usize),
    /// To test for corruption:
    /// * `rustup default stable-x86-64-pc-windows-msvc`
    /// * `cargo +nightly r -Zbuild-std --target x86_64-pc-windows-msvc -Zbuild-std-features=core/debug_refcell --features debug -- run PATH/TO/FILE.ms --verbose`
    Lookup(PrimitiveFlagsPair),
}

impl HeapPrimitive {
    pub const fn new_array_view(array: Rc<RefCell<Vec<Primitive>>>, index: usize) -> Self {
        Self::ArrayPtr(array, index)
    }

    pub const fn new_lookup_view(shared_ptr: PrimitiveFlagsPair) -> Self {
        Self::Lookup(shared_ptr)
    }

    pub(crate) fn to_owned_primitive(&self) -> Primitive {
        match self {
            Self::ArrayPtr(array, index) => array.borrow().get(*index).unwrap().to_owned(),
            Self::Lookup(cell) => cell.primitive().clone(),
        }
    }

    pub(crate) fn set(&self, new_val: Primitive) {
        match self {
            Self::Lookup(cell) => {
                cell.set_primitive(new_val);
            }
            Self::ArrayPtr(array, index) => {
                *array.borrow_mut().get_mut(*index).unwrap() = new_val;
            }
        }
    }

    pub(crate) fn update<F>(&self, setter: F) -> Result<Box<dyn Deref<Target = Primitive> + '_>>
    where
        F: FnOnce(&Primitive) -> Result<Primitive>,
    {
        match self {
            Self::Lookup(lookup) => {
                let new_value = lookup.update_primitive(setter)?;
                Ok(Box::new(new_value))
            }
            Self::ArrayPtr(array, index) => {
                let new_val = {
                    setter(array.borrow().get(*index).unwrap())?
                };

                let view = array.borrow_mut();

                let result = RefMut::map(view, |view| {
                    let view = view.get_mut(*index).unwrap();
                    *view = new_val;
                    view
                });

                
                Ok(Box::new(result))
            }
        }
    }

    fn borrow(&self) -> Box<dyn Deref<Target = Primitive> + '_> {
        match self {
            Self::Lookup(lookup) => {
                Box::new(lookup.primitive())
            }
            Self::ArrayPtr(array, index) => {
                let view = array.borrow();

                let result = Ref::map(view, |view| {
                    let view = view.get(*index).unwrap();
                    view
                });

                
                Box::new(result)
            }
        }
    }
}

impl PartialEq for HeapPrimitive {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ArrayPtr(a1, i1), Self::ArrayPtr(a2, i2)) => a1.borrow().eq(&a2.borrow()[..]) && i1 == i2,
            (Self::Lookup(lhs), Self::Lookup(rhs)) => lhs == rhs,
            _ => unimplemented!("not comparable"),
        }
    }
}

primitive! {
    Bool(bool),
    Str(String),
    Int(i32),
    BigInt(i128),
    Float(f64),
    Byte(u8),
    // We don't need reference counting because cloning a primitive function is cheap
    Function(crate::function::PrimitiveFunction),
    BuiltInFunction(BuiltInFunction),
    Vector(Rc<RefCell<Vec<crate::variables::Primitive>>>),
    HeapPrimitive(HeapPrimitive),
    Object(Rc<RefCell<crate::variables::Object>>),
    Module(Rc<RefCell<VariableMapping>>),
    // Supports Lazy Allocation
    Optional(Option<Box<crate::variables::Primitive>>),
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

        use Primitive as P;

        match (self, rhs) {
            (x, P::Optional(None)) | (P::Optional(None), x) => {
                return Ok(matches!(x, Primitive::Optional(None)))
            }
            (P::Vector(v1), P::Vector(v2)) => return Ok(v1.borrow().eq(v2.borrow().as_slice())),
            (P::Optional(maybe), yes) | (yes, P::Optional(maybe)) => {
                if let Some(maybe_unwrapped) = maybe {
                    return maybe_unwrapped.as_ref().equals(yes);
                };
            }
            _ => (),
        }

        impl_eq!(Int with Float(r=f64), BigInt(r=i128), Byte(r=i32));
        impl_eq!(Float with Int(r=f64), BigInt(r=f64), Byte(r=f64));
        impl_eq!(BigInt with Float(r=f64), Int(r=i128), Byte(r=i128));
        impl_eq!(Byte with Float(r=f64), BigInt(r=i128), Int(r=i32));

        impl_eq!(each Optional, Str, Bool, Function with itself);
    }

    /// Returns whether this primitive is numeric.
    pub fn is_numeric(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Int | Float | Byte | BigInt)
    }

    /// This function *must* be called before storing a primitive to any sort of
    /// long-term storage, as [`Primitive::HeapPrimitive`]s are inherently dangerous
    /// and should only be used for optimization/temporary register purposes.
    ///
    /// This code will blow up the VM if the HP ptr is not valid. If this happens, though,
    /// it is a bug with the compiler. Users will NEVER encounter a stale mutable pointer on
    /// their own, as it is a private type known only to the compiler used for array tricks.
    ///
    /// # Safety
    /// This function assumes that if `self` is a [HeapPrimitive]
    /// that points to memory inside a vector/list, the vector/list
    /// is "pinned" and has not been modified since this pointer's creation.
    /// Otherwise, this function will access a dangling reference and construct
    /// a Primitive from binary garbage.
    pub fn move_out_of_heap_primitive(self) -> Self {
        if let Self::HeapPrimitive(primitive) = self {
            primitive.to_owned_primitive()
        } else {
            self
        }
    }

    pub fn lookup(self, property: &str) -> std::result::Result<PrimitiveFlagsPair, Self> {
        use Primitive as P;
        match self.move_out_of_heap_primitive() {
            ret @ P::Object(..) => {
                let P::Object(ref obj) = ret else {
                    unreachable!()
                };

                let property = obj
                    .clone()
                    .borrow()
                    .get_property(property, true)
                    .ok_or(ret)?;
                Ok(property)
            }
            ret @ P::Module(..) => {
                let P::Module(ref module) = ret else {
                    unreachable!()
                };

                module.clone().borrow().get(property).ok_or(ret)
            }
            _ if property == "to_str" => Ok(PRIMITIVE_MODULE.generic_to_str()),
            ret @ P::Vector(..) => match property {
                "len" => Ok(PRIMITIVE_MODULE.vector_len()),
                "reverse" => Ok(PRIMITIVE_MODULE.vector_reverse()),
                "inner_capacity" => Ok(PRIMITIVE_MODULE.vector_inner_capacity()),
                "ensure_inner_capacity" => Ok(PRIMITIVE_MODULE.vector_ensure_inner_capacity()),
                "map" => Ok(PRIMITIVE_MODULE.vector_map()),
                "filter" => Ok(PRIMITIVE_MODULE.vector_filter()),
                "remove" => Ok(PRIMITIVE_MODULE.vector_remove()),
                "push" => Ok(PRIMITIVE_MODULE.vector_push()),
                "join" => Ok(PRIMITIVE_MODULE.vector_join()),
                "index_of" => Ok(PRIMITIVE_MODULE.vector_index_of()),
                _ => Err(ret),
            },
            ret @ P::Str(..) => match property {
                "len" => Ok(PRIMITIVE_MODULE.str_len()),
                "substring" => Ok(PRIMITIVE_MODULE.str_substring()),
                "contains" => Ok(PRIMITIVE_MODULE.str_contains()),
                "index_of" => Ok(PRIMITIVE_MODULE.str_index_of()),
                "inner_capacity" => Ok(PRIMITIVE_MODULE.str_inner_capacity()),
                "reverse" => Ok(PRIMITIVE_MODULE.str_reverse()),
                "insert" => Ok(PRIMITIVE_MODULE.str_insert()),
                "replace" => Ok(PRIMITIVE_MODULE.str_replace()),
                "delete" => Ok(PRIMITIVE_MODULE.str_delete()),
                "parse_int" => Ok(PRIMITIVE_MODULE.str_parse_int()),
                "parse_int_radix" => Ok(PRIMITIVE_MODULE.str_parse_int_radix()),
                "parse_bigint" => Ok(PRIMITIVE_MODULE.str_parse_bigint()),
                "parse_bigint_radix" => Ok(PRIMITIVE_MODULE.str_parse_bigint_radix()),
                "parse_bool" => Ok(PRIMITIVE_MODULE.str_parse_bool()),
                "parse_float" => Ok(PRIMITIVE_MODULE.str_parse_float()),
                "parse_byte" => Ok(PRIMITIVE_MODULE.str_parse_byte()),
                "split" => Ok(PRIMITIVE_MODULE.str_split()),
                _ => Err(ret),
            },
            ret @ (P::Int(..) | P::BigInt(..) | P::Float(..) | P::Byte(..)) => match property {
                "pow" => Ok(PRIMITIVE_MODULE.generic_num_pow()),
                "powf" => Ok(PRIMITIVE_MODULE.generic_num_powf()),
                "sqrt" => Ok(PRIMITIVE_MODULE.generic_num_sqrt()),
                "to_int" => Ok(PRIMITIVE_MODULE.generic_num_to_int()),
                "to_bigint" => Ok(PRIMITIVE_MODULE.generic_num_to_bigint()),
                "to_byte" => Ok(PRIMITIVE_MODULE.generic_num_to_byte()),
                "to_float" => Ok(PRIMITIVE_MODULE.generic_num_to_float()),
                "abs" => Ok(PRIMITIVE_MODULE.generic_num_abs()),
                "to_ascii" => Ok(PRIMITIVE_MODULE.byte_to_ascii()),
                "fpart" => Ok(PRIMITIVE_MODULE.float_fpart()),
                "ipart" => Ok(PRIMITIVE_MODULE.float_ipart()),
                "round" => Ok(PRIMITIVE_MODULE.float_round()),
                "floor" => Ok(PRIMITIVE_MODULE.float_floor()),
                "ceil" => Ok(PRIMITIVE_MODULE.float_ceil()),
                _ => Err(ret),
            },
            ret @ P::Function(..) => match property {
                "is_closure" => Ok(PRIMITIVE_MODULE.fn_is_closure()),
                _ => Err(ret),
            },
            ret => Err(ret),
        }
    }

    /// Returns whether this primitive is a numeric coalesce.
    pub fn is_numeric_coalesce(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Bool | Int | Float | Byte | BigInt)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.ty(), Type::Function)
    }

    pub fn try_into_numeric_index(&self) -> Result<usize> {
        Ok(match self {
            Primitive::Byte(byte) => *byte as usize,
            Primitive::BigInt(bigint) => *bigint as usize,
            Primitive::Int(int) => *int as usize,
            other => bail!("cannot index with {other}"),
        })
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

    fn fmt_recursive(&self, f: &mut std::fmt::Formatter, mut depth: u8) -> std::fmt::Result {
        use Primitive::*;

        match self {
            Bool(b) => write!(f, "{}", *b),
            Str(s) if depth != 0 => write!(f, "\"{s}\""),
            Str(s) => write!(f, "{s}"),
            Int(n) => write!(f, "{n}"),
            BigInt(n) => write!(f, "{n}"),
            Float(n) => write!(f, "{n}"),
            Byte(b) => write!(f, "0b{:b}", *b),
            Function(fun) => write!(f, "{fun}"),
            BuiltInFunction(name) => write!(f, "<static {name:?}>"),
            Vector(l) => {
                write!(f, "[")?;
                let borrow = l.borrow();
                let mut iter = borrow.iter();

                depth += 1;

                if let Some(first) = iter.next() {
                    first.fmt_recursive(f, depth)?;
                }

                for value in iter {
                    write!(f, ", ")?;
                    value.fmt_recursive(f, depth)?;
                }

                write!(f, "]")
            }
            Module(module) => {
                if cfg!(feature = "debug") {
                    write!(f, "module {:#?}", module.borrow())
                } else {
                    write!(f, "<module @ {:#x}>", module.as_ptr() as usize)
                }
            }
            HeapPrimitive(hp) => {
                let view = hp.borrow();
                let view = view.deref().deref();
                if cfg!(feature = "debug") {
                    write!(f, "&{view}")
                } else {
                    write!(f, "{view}")
                }
            }
            Object(o) => write!(f, "{}", o.borrow()),
            Optional(Some(primitive)) => write!(f, "{primitive}"),
            Optional(None) => write!(f, "nil"),
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_recursive(f, 0)
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
    use super::Primitive;
    use crate::{bigint, bool, byte, float, int, string};
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
