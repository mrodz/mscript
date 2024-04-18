//! The interpreter's primitive datatypes.
//! Every "value" in the interpreter is a primitive.

use crate::{
    bigint, bool, byte,
    file::ExportMap,
    float,
    function::{BuiltInFunction, PrimitiveFunction},
    int,
    stack::{PrimitiveFlagsPair, VariableMapping, PRIMITIVE_MODULE},
    string,
};
use anyhow::{bail, Context, Result};
use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

/// This macro allows easy recursion over variants.
macro_rules! primitive {
    ($($variant:ident($type:ty)),+ $(,)?) => {
        /// Every possible form of variable data used in the interpreter.
        #[derive(PartialEq, Debug, Clone, Trace, Finalize)]
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

#[derive(Debug, Clone, Trace, Finalize)]
pub enum HeapPrimitive {
    ArrayPtr(GcVector, usize),
    /// To test for corruption:
    /// * `rustup default stable-x86-64-pc-windows-msvc`
    /// * `cargo +nightly r -Zbuild-std --target x86_64-pc-windows-msvc -Zbuild-std-features=core/debug_refcell --features debug -- run PATH/TO/FILE.ms --verbose`
    Lookup(PrimitiveFlagsPair),
    MapPtr(GcMap, Box<Primitive>),
}

impl HeapPrimitive {
    pub const fn new_array_view(array: GcVector, index: usize) -> Self {
        Self::ArrayPtr(array, index)
    }

    pub const fn new_lookup_view(shared_ptr: PrimitiveFlagsPair) -> Self {
        Self::Lookup(shared_ptr)
    }

    pub(crate) fn to_owned_primitive(&self) -> Result<Primitive> {
        match self {
            Self::ArrayPtr(array, index) => Ok(array.0.borrow().get(*index).unwrap().to_owned()),
            Self::Lookup(cell) => Ok(cell.primitive().clone()),
            Self::MapPtr(map, index) => map.get((**index).clone()).context("key error: map does not have key"),
        }
    }

    pub(crate) fn set(&self, new_val: Primitive) -> Result<()> {
        match self {
            Self::Lookup(cell) => {
                cell.set_primitive(new_val);
            }
            Self::ArrayPtr(array, index) => {
                *array.0.borrow_mut().get_mut(*index).unwrap() = new_val;
            }
            Self::MapPtr(map, key) => {
                map.insert((**key).clone(), new_val)?;
            }
        }

        Ok(())
    }

    pub(crate) fn update(
        &self,
        setter: impl FnOnce(&dyn Deref<Target = Primitive>) -> Result<Primitive>,
    ) -> Result<Primitive> {
        match self {
            Self::Lookup(lookup) => {
                let new_value = lookup.update_primitive(setter)?;
                Ok(new_value.to_owned())
                // Ok(Box::new(new_value))
            }
            Self::ArrayPtr(array, index) => {
                let new_val = { setter(&array.0.borrow().get(*index).unwrap())? };
                let view = array.0.borrow_mut();

                let result = GcCellRefMut::map(view, |view| {
                    let view = view.get_mut(*index).unwrap();
                    *view = new_val;
                    view
                });

                // let result = array.0.borrow().get(*index).unwrap();

                // Ok(Box::new(result.to_owned()))
                Ok(result.to_owned())
            }
            Self::MapPtr(map, key) => {
                let new_val =
                    { setter(&&map.get((**key).clone()).context("key error: map does not have key")?)? };
                map.insert((**key).clone(), new_val)?;
                Ok(map.get((**key).clone()).unwrap())
            }
        }
    }

    fn borrow(&self) -> Result<Box<dyn Deref<Target = Primitive> + '_>> {
        match self {
            Self::Lookup(lookup) => Ok(Box::new(lookup.primitive())),
            Self::ArrayPtr(array, index) => {
                Ok(Box::new(GcCellRef::map(array.0.borrow(), |view| {
                    view.get(*index).unwrap()
                })))
            }
            Self::MapPtr(map, key) => Ok(Box::new(Box::new(
                map.get((**key).clone()).context("key error: map does not have key")?,
            ))),
        }
    }
}

impl PartialEq for HeapPrimitive {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::ArrayPtr(a1, i1), Self::ArrayPtr(a2, i2)) => a1.0.eq(&a2.0) && i1 == i2,
            (Self::Lookup(lhs), Self::Lookup(rhs)) => lhs == rhs,
            _ => unimplemented!("not comparable"),
        }
    }
}

#[derive(Clone, Trace, Finalize, Debug, Default)]
pub struct GcMap(pub(crate) Gc<GcCell<HashMap<Primitive, Primitive>>>);

impl PartialEq for GcMap {
    /// You cannot compare a hash map
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl GcMap {
    #[allow(clippy::mutable_key_type)]
    pub fn new(raw_map: HashMap<Primitive, Primitive>) -> Self {
        Self(Gc::new(GcCell::new(raw_map)))
    }

    pub fn addr(&self) -> *const HashMap<Primitive, Primitive> {
        let view = self.0.borrow();
        &*view as *const _
    }

    pub fn insert(&self, key: Primitive, value: Primitive) -> Result<Option<Primitive>> {
        let mut view = self.0.borrow_mut();
        Ok(view.insert(
            key.move_out_of_heap_primitive()?,
            value.move_out_of_heap_primitive()?,
        ))
    }

    pub fn get(&self, key: Primitive) -> Result<Primitive> {
        let view = self.0.borrow();
        view.get(&key.move_out_of_heap_primitive()?).cloned().context("could not read from view")
    }
}

#[derive(Clone, Trace, Finalize, Debug, Default)]
pub struct GcVector(pub(crate) Gc<GcCell<Vec<Primitive>>>);

impl Hash for GcVector {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let view = self.0.borrow();
        view.hash(state)
    }
}

impl GcVector {
    pub fn new(vec: Vec<Primitive>) -> Self {
        Self(Gc::new(GcCell::new(vec)))
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Gc::new(GcCell::new(Vec::with_capacity(capacity))))
    }

    pub fn addr(&self) -> *const Primitive {
        self.0.borrow().as_ptr()
    }
}

impl PartialEq for GcVector {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

#[derive(Trace, Finalize, Clone, Debug, PartialEq, Eq, Hash)]
pub struct NonSweepingBuiltInFunction(#[unsafe_ignore_trace] pub(crate) BuiltInFunction);

impl Deref for NonSweepingBuiltInFunction {
    type Target = BuiltInFunction;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for NonSweepingBuiltInFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
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
    Function(PrimitiveFunction),
    BuiltInFunction(NonSweepingBuiltInFunction),
    Vector(GcVector),
    HeapPrimitive(HeapPrimitive),
    Object(crate::variables::Object),
    Module(ExportMap),
    // Supports Lazy Allocation
    Optional(Option<Box<crate::variables::Primitive>>),
    Map(GcMap),
}

/// https://github.com/rust-lang/rust/blob/5c674a11471ec0569f616854d715941757a48a0a/src/libcore/num/f64.rs#L203-L216
fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = val.to_bits();
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

impl Hash for Primitive {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use Primitive::*;

        match self
            .move_out_of_heap_primitive_borrow()
            .unwrap_or(Cow::Owned(Primitive::Optional(None)))
            .as_ref()
        {
            Int(x) => x.hash(state),
            Bool(x) => x.hash(state),
            BigInt(x) => x.hash(state),
            BuiltInFunction(x) => x.hash(state),
            Byte(x) => x.hash(state),
            Float(x) => integer_decode(*x).hash(state),
            Function(x) => x.hash(state),
            Map(_) => unimplemented!("you may not use a map as a key"),
            Object(x) => x.hash(state),
            Vector(x) => x.hash(state),
            Str(x) => x.hash(state),
            Module(x) => (&*x.borrow() as *const VariableMapping as usize).hash(state),
            Optional(x) => x.hash(state),
            HeapPrimitive(_) => unreachable!(),
        }
    }
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
                        (Primitive::$lhs_and_rhs(x), Primitive::$lhs_and_rhs(y)) => return Ok(x.eq(y)),
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
            (P::Vector(v1), P::Vector(v2)) => {
                return Ok(v1.0.borrow()[..].eq(v2.0.borrow().as_slice()))
            }
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

    pub fn runtime_addr_check(&self, rhs: &Self) -> Result<Primitive> {
        match (self, rhs) {
            (Self::BuiltInFunction(id1), Self::BuiltInFunction(id2)) => {
                return Ok(Primitive::Bool(id1 == id2))
            }
            (Self::Object(o1), Self::Object(o2)) => {
                return Ok(Primitive::Bool(o1.id_addr() == o2.id_addr()))
            }
            (Self::Module(m1), Self::Module(m2)) => return Ok(Primitive::Bool(m1 == m2)),
            (Self::Function(f1), Self::Function(f2)) => return Ok(Primitive::Bool(f1 == f2)),
            (Self::Vector(v1), Self::Vector(v2)) => {
                return Ok(Primitive::Bool(v1.addr() == v2.addr()))
            }
            (Self::Map(m1), Self::Map(m2)) => return Ok(Primitive::Bool(m1.addr() == m2.addr())),
            _ => (),
        }

        self.equals(rhs).map(Primitive::Bool)
    }

    /// Returns whether this primitive is numeric.
    pub fn is_numeric(&self) -> bool {
        use Type::*;

        matches!(self.ty(), Int | Float | Byte | BigInt)
    }

    pub fn move_out_of_heap_primitive(self) -> Result<Self> {
        if let Self::HeapPrimitive(ref primitive) = self {
            Ok(primitive.to_owned_primitive()?)
        } else {
            Ok(self)
        }
    }

    pub fn move_out_of_heap_primitive_borrow(&self) -> Result<Cow<Self>> {
        if let Self::HeapPrimitive(primitive) = self {
            Ok(Cow::Owned(primitive.to_owned_primitive()?))
        } else {
            Ok(Cow::Borrowed(self))
        }
    }

    pub fn lookup(self, property: &str) -> Result<std::result::Result<PrimitiveFlagsPair, Self>> {
        use Primitive as P;
        match self.move_out_of_heap_primitive()? {
            ret @ P::Object(..) => {
                let P::Object(ref obj) = ret else {
                    unreachable!()
                };

                match obj.get_property(property, true).ok_or(ret) {
                    Ok(property) => Ok(Ok(property)),
                    Err(this) => Ok(Err(this)),
                }
            }
            ret @ P::Module(..) => {
                let P::Module(ref module) = ret else {
                    unreachable!()
                };

                let module = module.borrow();

                if let Some(property_value) = module.get(property) {
                    Ok(Ok(property_value))
                } else {
                    drop(module);
                    Ok(Err(ret))
                }
            }
            _ if property == "to_str" => Ok(Ok(PRIMITIVE_MODULE.generic_to_str())),
            ret @ P::Vector(..) => Ok(match property {
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
            }),
            ret @ P::Str(..) => Ok(match property {
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
            }),
            ret @ (P::Int(..) | P::BigInt(..) | P::Float(..) | P::Byte(..)) => Ok(match property {
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
            }),
            ret @ P::Function(..) => Ok(match property {
                "is_closure" => Ok(PRIMITIVE_MODULE.fn_is_closure()),
                _ => Err(ret),
            }),
            ret => Ok(Err(ret)),
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

        let offset = if string.len() >= 3 && bytes[..2] == [b'0', b'b'] {
            (2, 2)
        } else {
            (0, 10)
        };

        let byte = u8::from_str_radix(&string[offset.0..], offset.1)
            .expect("malformed byte. format: (0b10101010)");

        Ok(byte!(byte))
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
                let borrow = &l.0.borrow();
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
                write!(f, "module {:#?}", module)

                // if cfg!(feature = "debug") {
                //     write!(f, "module {:#?}", module)
                // } else {
                //     write!(f, "<module @ {:#x}>", module as *const _ as usize)
                // }
            }
            HeapPrimitive(hp) => {
                let none = Primitive::Optional(None);
                let view = hp.borrow().unwrap_or(Box::new(&none));
                let view = view.deref().deref();
                if cfg!(feature = "debug") {
                    write!(f, "&{view}")
                } else {
                    write!(f, "{view}")
                }
            }
            Object(o) => write!(f, "{o}"),
            Optional(Some(primitive)) => write!(f, "{primitive}"),
            Optional(None) => write!(f, "nil"),
            Map(map) => {
                write!(f, "{{")?;

                let map = map.0.borrow();
                let mut map_iter = map.iter();

                depth += 1;

                if let Some((key, value)) = map_iter.next() {
                    key.fmt_recursive(f, depth)?;
                    write!(f, ": ")?;
                    value.fmt_recursive(f, depth)?;
                }

                for (key, value) in map_iter {
                    write!(f, ", ")?;
                    key.fmt_recursive(f, depth)?;
                    write!(f, ": ")?;
                    value.fmt_recursive(f, depth)?;
                }

                write!(f, "}}")
            }
        }
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_recursive(f, 0)
    }
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
