#[macro_export]
macro_rules! string {
    ($data:expr) => {
        Primitive::Str(buckets::Str($data))
    };
    (raw $data:expr) => {{
        Primitive::Str(buckets::Str(Into::<String>::into($data)))
    }};
}

#[macro_export]
macro_rules! bigint {
    ($data:expr) => {
        Primitive::BigInt(buckets::BigInt($data))
    };
}

#[macro_export]
macro_rules! int {
    ($data:expr) => {
        Primitive::Int(buckets::Int($data))
    };
}

#[macro_export]
macro_rules! float {
    ($data:expr) => {
        Primitive::Float(buckets::Float($data))
    };
}

#[macro_export]
macro_rules! bool {
    ($data:expr) => {
        Primitive::Bool(buckets::Bool($data))
    };
}

#[macro_export]
macro_rules! char {
    ($data:expr) => {
        Primitive::Char(buckets::Char($data))
    };
}

#[macro_export]
macro_rules! byte {
    ($data:expr) => {
        Primitive::Byte(buckets::Byte($data))
    };
}

#[macro_export]
macro_rules! function {
    ($data:expr) => {
        Primitive::Function(buckets::Function($data))
    };
}

#[macro_export]
macro_rules! vector {
    () => {
        Primitive::Vector(buckets::Vector(vec![]))
    };
    ($elem:expr; $n:expr) => {
        Primitive::Vector(buckets::Vector(vec![$elem; $n]))
    };
    ($($x:expr),+ $(,)?) => {
        {
            let mut vector = vec![];
            $(
                vector.push($x);
            )*
            Primitive::Vector(buckets::Vector(vector))
        }
    };
	(raw $data:expr) => {
		Primitive::Vector(buckets::Vector($data))
	};
}

#[cfg(test)]
mod test {
    use crate::bytecode::variables::{buckets, Primitive};
    use crate::*;

    #[test]
    pub fn ints() {
        assert_eq!(int!(5), Primitive::Int(buckets::Int(5)))
    }

    #[test]
    pub fn vectors() {
        let vector = Primitive::Vector(buckets::Vector(vec![
            Primitive::Int(buckets::Int(5)),
            Primitive::Str(buckets::Str("Hello".into())),
            Primitive::Vector(buckets::Vector(vec![Primitive::Float(buckets::Float(
                3.14159,
            ))])),
        ]));

        assert_eq!(
            vector![int!(5), string!(raw "Hello"), vector![float!(3.14159)]],
            vector
        )
    }
}
