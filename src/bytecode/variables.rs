mod equality;
mod variable;

pub use variable::bin_op_from;
pub use variable::bin_op_result;
pub use variable::buckets;
pub use variable::Primitive;
pub use variable::Variable;

pub mod primitive_shorthands {
    #[macro_export]
    macro_rules! string {
        ($data:expr) => {
            Primitive::Str(buckets::Str($data))
        };
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
}