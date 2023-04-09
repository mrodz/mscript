use anyhow::{bail, Result};

use crate::{
    apply_math_bin_op_if_applicable,
    variables::Primitive::{self, *},
};

impl std::ops::Rem for Primitive {
    type Output = Result<Primitive>;

    fn rem(self, rhs: Self) -> Self::Output {
        match rhs {
            Int(0) | BigInt(0) => bail!("% by 0"),
            Float(f) if f == 0.0 => bail!("% by 0"),
            _ => (),
        }

        let (t1, t2) = (&self, &rhs);

        let math = apply_math_bin_op_if_applicable!(t1 % t2);

        if let Some(result) = math {
            Ok(result)
        } else {
            bail!("valid ops: number % number")
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn one_hundred_mod_six() {
        let result = int!(100) % float!(6.0);
        assert_eq!(result.unwrap(), float!(4.0))
    }

    #[test]
    fn twenty_mod_two() {
        let result = bigint!(20) % int!(2);
        assert_eq!(result.unwrap(), bigint!(0))
    }

    #[test]
    fn negative_eight_mod_three() {
        let result = bigint!(-8) % float!(3.0);
        assert_eq!(result.unwrap(), float!(-2.0))
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let result = int!(1) % int!(0);
        result.unwrap();
    }
}
