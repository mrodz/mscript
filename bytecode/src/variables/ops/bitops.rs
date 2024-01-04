use crate::variables::Primitive::{self, *};
use crate::*;
use anyhow::{bail, Context, Result};

macro_rules! generic_bitop {
    (@tests $name:ident $symbol:tt) => {
        #[cfg(test)]
        mod $name {
            use super::*;

            #[test]
            fn round_one() {
                let result = byte!(0b101) $symbol byte!(0b010);
                assert_eq!(result.unwrap(), byte!(0b101 $symbol 0b010))
            }

            #[test]
            fn round_two() {
                let result = bigint!(2) $symbol int!(200);
                assert_eq!(result.unwrap(), bigint!(2 $symbol 200))
            }

            #[test]
            fn round_three() {
                let result = bigint!(1) $symbol bigint!(-1_000);
                assert_eq!(result.unwrap(), bigint!(1 $symbol -1_000))
            }
        }
    };
    ($trait:ident::$fn:ident, $symbol:tt) => {
        impl std::ops::$trait for Primitive {
            type Output = Result<Primitive>;
            fn $fn(self, rhs: Self) -> Self::Output {
                &self $symbol &rhs
            }
        }

        impl std::ops::$trait for &Primitive {
            type Output = Result<Primitive>;
            fn $fn(self, rhs: Self) -> Self::Output {
                let (t1, t2) = (&self, &rhs);
                apply_math_bin_op_if_applicable!(@no_f64 t1 $symbol t2).with_context(|| format!("<{:?} {} {:?}> is invalid. (valid ops are: <non-float {} non-float>)", t1.ty(), stringify!($symbol), t2.ty(), stringify!($symbol)))
            }
        }

        generic_bitop!(@tests $fn $symbol);
    };
    (@checked $trait:ident::$fn:ident, $symbol:tt, safe=$checked:ident) => {
        impl std::ops::$trait for Primitive {
            type Output = Result<Primitive>;
            fn $fn(self, rhs: Self) -> Self::Output {
                &self $symbol &rhs
            }
        }

        impl std::ops::$trait for &Primitive {
            type Output = Result<Primitive>;
            fn $fn(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Int(x), Int(y)) => Ok(int!(i32::$checked(*x, (*y).try_into()?).context("operation overflow/underflow")?)),
                    (Int(x), BigInt(y)) => Ok(bigint!(i128::$checked(*x as i128, (*y).try_into()?).context("operation overflow/underflow")?)),
                    (Int(x), Byte(y)) => Ok(int!(i32::$checked(*x, *y as u32).context("operation overflow/underflow")?)),
                    (BigInt(x), BigInt(y)) => Ok(bigint!(i128::$checked(*x, (*y).try_into()?).context("operation overflow/underflow")?)),
                    (BigInt(x), Int(y)) => Ok(bigint!(i128::$checked(*x, (*y).try_into()?).context("operation overflow/underflow")?)),
                    (BigInt(x), Byte(y)) => Ok(bigint!(i128::$checked(*x, *y as u32).context("operation overflow/underflow")?)),
                    (Byte(x), Byte(y)) => Ok(byte!(u8::$checked(*x, *y as u32).context("operation overflow/underflow")?)),
                    (Byte(x), Int(y)) => Ok(int!(i32::$checked(*x as i32, (*y).try_into()?).context("operation overflow/underflow")?)),
                    (Byte(x), BigInt(y)) => Ok(bigint!(i128::$checked(*x as i128, (*y).try_into()?).context("operation overflow/underflow")?)),

                    _ => bail!("<{:?} {} {:?}> is invalid. (valid ops are: <non-float {} non-float>)", self.ty(), stringify!($symbol), rhs.ty(), stringify!($symbol))
                }
            }
        }

        #[cfg(test)]
        mod $fn {
            use super::*;

            #[test]
            fn round_one() {
                let result = byte!(0b101) $symbol byte!(0b010);
                assert_eq!(result.unwrap(), byte!(0b101 $symbol 0b010))
            }

            #[test]
            #[should_panic]
            fn round_two() {
                let result = bigint!(2) $symbol int!(200);
                assert_eq!(result.unwrap(), bigint!(2 $symbol 200))
            }

            #[test]
            #[should_panic]
            fn round_three() {
                let result = bigint!(1) $symbol bigint!(-1_000);
                assert_eq!(result.unwrap(), bigint!(1 $symbol -1_000))
            }

            #[test]
            fn round_four() {
                let result = int!(32) $symbol byte!(2);
                assert_eq!(result.unwrap(), int!(32 $symbol 2))
            }
        }
    }
}

generic_bitop!(BitAnd::bitand, &);
generic_bitop!(BitOr::bitor, |);
generic_bitop!(BitXor::bitxor, ^);
generic_bitop!(@checked Shl::shl, <<, safe=checked_shl);
generic_bitop!(@checked Shr::shr, >>, safe=checked_shr);
