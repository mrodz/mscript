#[macro_export]
macro_rules! string {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Str($data)
    }};
    (raw $data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Str(Into::<String>::into($data))
    }};
}

#[macro_export]
macro_rules! bigint {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::BigInt($data)
    }};
}

#[macro_export]
macro_rules! int {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Int($data)
    }};
}

#[macro_export]
macro_rules! float {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Float($data)
    }};
}

#[macro_export]
macro_rules! bool {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Bool($data)
    }};
}

#[macro_export]
macro_rules! byte {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Byte($data)
    }};
}

#[macro_export]
macro_rules! function {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Function($data)
    }};
}

#[macro_export]
macro_rules! vector {
    () => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Vector(vec![])
    }};
    ($elem:expr; $n:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Vector(vec![$elem; $n])
    }};
    ($($x:expr),+ $(,)?) => {{
        use crate::BytecodePrimitive;

        let mut vector = vec![];
        $(
            vector.push($x);
        )*
        BytecodePrimitive::Vector(vector)
    }};
	(raw $data:expr) => {{
        use crate::BytecodePrimitive;

		BytecodePrimitive::Vector($data)
	}};
}

#[macro_export]
macro_rules! object {
    ($data:expr) => {{
        use crate::BytecodePrimitive;

        BytecodePrimitive::Object($data)
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
        let vector = BytecodePrimitive::Vector(vec![
            BytecodePrimitive::Int(5),
            BytecodePrimitive::Str("Hello".into()),
            BytecodePrimitive::Vector(vec![BytecodePrimitive::Float(3.14159)]),
        ]);

        assert_eq!(
            vector![int!(5), string!(raw "Hello"), vector![float!(3.14159)]],
            vector
        )
    }
}
