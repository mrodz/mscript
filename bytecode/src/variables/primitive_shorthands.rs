#[macro_export]
macro_rules! string {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Str($data)
    }};
    (raw $data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Str(Into::<String>::into($data))
    }};
}

#[macro_export]
macro_rules! bigint {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::BigInt($data)
    }};
}

#[macro_export]
macro_rules! int {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Int($data)
    }};
}

#[macro_export]
macro_rules! float {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Float($data)
    }};
}

#[macro_export]
macro_rules! bool {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Bool($data)
    }};
}

#[macro_export]
macro_rules! byte {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Byte($data)
    }};
}

#[macro_export]
macro_rules! function {
    ($data:expr) => {{
        use crate::variables::Primitive;

        Primitive::Function($data)
    }};
}

#[macro_export]
macro_rules! vector {
    () => {{
        use crate::variables::Primitive;

        Primitive::Vector(vec![])
    }};
    ($elem:expr; $n:expr) => {{
        use crate::variables::Primitive;

        Primitive::Vector(vec![$elem; $n])
    }};
    ($($x:expr),+ $(,)?) => {{
        use crate::variables::Primitive;

        let mut vector = vec![];
        $(
            vector.push($x);
        )*
        Primitive::Vector(vector)
    }};
	(raw $data:expr) => {{
        use crate::variables::Primitive;

		Primitive::Vector($data)
	}};
}

#[macro_export]
macro_rules! object {
    ($data:expr) => {
        Primitive::Object($data)
    };
}

#[cfg(test)]
mod test {
    use crate::{variables::Primitive, *};

    #[test]
    pub fn ints() {
        assert_eq!(int!(5), Primitive::Int(5))
    }

    #[test]
    pub fn vectors() {
        let vector = Primitive::Vector(vec![
            Primitive::Int(5),
            Primitive::Str("Hello".into()),
            Primitive::Vector(vec![Primitive::Float(3.14159)]),
        ]);

        assert_eq!(
            vector![int!(5), string!(raw "Hello"), vector![float!(3.14159)]],
            vector
        )
    }
}
