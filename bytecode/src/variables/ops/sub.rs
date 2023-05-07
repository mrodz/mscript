use crate::variables::Primitive;
use crate::*;
use anyhow::{bail, Result};

impl std::ops::Sub for Primitive {
    type Output = Result<Primitive>;

    fn sub(self, rhs: Self) -> Self::Output {
        let (t1, t2) = (&self, &rhs);
        let math = apply_math_bin_op_if_applicable!(t1 - t2);

        if let Some(result) = math {
            Ok(result)
        } else {
            bail!("valid ops: number - number")
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn five_minus_ten() {
        let result = int!(5) - float!(10.0);
        assert_eq!(result.unwrap(), float!(-5.0))
    }

    #[test]
    fn two_minus_two_hundred() {
        let result = bigint!(2) - int!(200);
        assert_eq!(result.unwrap(), bigint!(-198))
    }

    #[test]
    fn one_minus_negative_one_thousand() {
        let result = bigint!(1) - float!(-1_000.0);
        assert_eq!(result.unwrap(), float!(1001.0))
    }

    #[test]
    fn integer_limit_minus_one() {
        let result = int!(i32::MAX) - int!(1);
        assert_eq!(result.unwrap(), int!(i32::MAX - 1))
    }
}
