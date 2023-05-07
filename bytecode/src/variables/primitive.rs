macro_rules! primitive {
    ($($variant:ident($type:ty)),+ $(,)?) => {
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
    Function(std::sync::Arc<crate::function::PrimitiveFunction>),
    Vector(std::sync::Arc<Vec<crate::variables::Primitive>>),
    Object(std::sync::Arc<crate::variables::Object>),
}
