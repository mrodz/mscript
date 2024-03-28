/// Create a bytecode String primitive.
/// ```
/// use bytecode::string;
/// let s1 = string!(String::from("Hello World"));
/// let s2 = string!(raw "Hello World");
/// assert_eq!(s1, s2);
/// ```
#[macro_export]
macro_rules! string {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Str($data)
    }};
    (raw $data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Str(Into::<String>::into($data))
    }};
}

/// Create a bytecode BigInt primitive.
/// ```
/// use bytecode::bigint;
/// let b = bigint!(0xFEED_BEEF_FEED_BEEF);
/// ```
#[macro_export]
macro_rules! bigint {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::BigInt($data)
    }};
}

/// Create a bytecode Int primitive
/// ```
/// use bytecode::int;
/// let i = int!(5);
/// ```
#[macro_export]
macro_rules! int {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Int($data)
    }};
}

/// Create a bytecode Float primitive
/// ```
/// use bytecode::float;
/// let pi = float!(3.141592);
/// ```
#[macro_export]
macro_rules! float {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Float($data)
    }};
}

/// Create a bytecode Bool primitive
/// ```
/// use bytecode::bool;
/// let truthy = bool!(true);
/// ```
#[macro_export]
macro_rules! bool {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Bool($data)
    }};
}

/// Create a bytecode Byte primitive
/// ```
/// use bytecode::byte;
/// let five = byte!(0b101);
/// ```
#[macro_export]
macro_rules! byte {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Byte($data)
    }};
}

#[macro_export]
macro_rules! function {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Function($data)
    }};
}

/// Create a bytecode Function primitive
/// ```
/// use bytecode::*;
/// let empty = vector!();
/// let three_fives = vector!(int!(5); 3);
/// let fun_numbers = vector![string!(raw "hello"), int!(2), float!(20.333)];
/// let from_vec = vector!(raw vec![int!(100)]);
/// ```
#[macro_export]
macro_rules! vector {
    () => {{
        use $crate::{BytecodePrimitive, GcVector};

        BytecodePrimitive::Vector(GcVector::new(vec![]))
    }};
    ($elem:expr; $n:expr) => {{
        use $crate::{BytecodePrimitive, GcVector};

        BytecodePrimitive::Vector(GcVector::new(vec![$elem; $n]))
    }};
    ($($x:expr),+ $(,)?) => {{
        use $crate::{BytecodePrimitive, GcVector};
        BytecodePrimitive::Vector(GcVector::new(vec![$($x,)*]))
    }};
    (raw $data:expr) => {{
        use $crate::{BytecodePrimitive, GcVector};

        BytecodePrimitive::Vector(GcVector::new($data))
    }};
}

#[macro_export]
macro_rules! object {
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Object($data)
    }};
}

/// Create a bytecode optional primitive
#[macro_export]
macro_rules! optional {
    (empty) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Optional(None)
    }};
    ($data:expr) => {{
        use $crate::BytecodePrimitive;

        BytecodePrimitive::Optional(Some(Box::new($data)))
    }};
}

#[cfg(test)]
mod test {
    use crate::{BytecodePrimitive, *};

    #[test]
    pub fn ints() {
        assert_eq!(int!(5), BytecodePrimitive::Int(5))
    }

    #[test]
    pub fn vectors() {
        let vector = BytecodePrimitive::Vector(GcVector::new(vec![
            BytecodePrimitive::Int(5),
            BytecodePrimitive::Str("Hello".into()),
            BytecodePrimitive::Vector(GcVector::new(vec![BytecodePrimitive::Float(3.14159)])),
        ]));

        assert_eq!(
            vector![int!(5), string!(raw "Hello"), vector![float!(3.14159)]],
            vector
        )
    }
}
