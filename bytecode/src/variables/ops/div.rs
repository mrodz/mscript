use crate::variables::Primitive::*;
use crate::*;
use anyhow::{bail, Result};

impl std::ops::Div for Primitive {
    type Output = Result<Primitive>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

impl std::ops::Div for &Primitive {
    type Output = Result<Primitive>;

    fn div(self, rhs: Self) -> Self::Output {
        match rhs {
            Int(0) | BigInt(0) => bail!("/ by 0"),
            Float(f) if f == &0.0 => {
                log::error!("{self} / 0");

                bail!("/ by 0")
            }
            _ => (),
        }

        let (t1, t2) = (&self, &rhs);
        let math = apply_math_bin_op_if_applicable!(t1 / t2);

        if let Some(result) = math {
            Ok(result)
        } else {
            bail!(
                "<{:?} / {:?}> is invalid. (valid ops are: <num / num>)",
                t1.ty(),
                t2.ty()
            );
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn one_hundred_divided_by_four() {
        let result = int!(100) / float!(4.0);
        assert_eq!(result.unwrap(), float!(25.0))
    }

    #[test]
    fn twenty_divided_by_two() {
        let result = bigint!(20) / int!(2);
        assert_eq!(result.unwrap(), bigint!(10))
    }

    #[test]
    fn negative_eight_divided_by_three() {
        let result = bigint!(-8) / float!(3.0);
        assert_eq!(result.unwrap(), float!(-8.0 / 3.0))
    }

    #[test]
    #[should_panic]
    fn div_by_zero() {
        let result = int!(1) / int!(0);
        result.unwrap();
    }
}
