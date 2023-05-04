macro_rules! primitive {
    ($($variant:ident($type:ty)),+ $(,)?) => {
        // pub mod buckets {
        //     $(
        //         #[derive(PartialEq, PartialOrd, Clone)]
        //         pub struct $variant(pub(crate) $type);

        //         impl std::fmt::Debug for $variant {
        //             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        //                 write!(f, "{:?}", self.0)
        //             }
        //         }

        //         impl std::fmt::Display for $variant {
        //             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        //                 write!(f, "{:?}", self.0)
        //             }
        //         }

        //         impl std::ops::Deref for $variant {
        //             type Target = $type;
        //             fn deref(&self) -> &Self::Target {
        //                 &self.0
        //             }
        //         }
        //     )*
        // }

        #[derive(PartialEq, PartialOrd, Debug, Clone)]
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
    Function(crate::bytecode::function::PrimitiveFunction),
    Vector(Vec<crate::bytecode::variables::Primitive>),
    Object(std::sync::Arc<crate::bytecode::variables::Object>),
}