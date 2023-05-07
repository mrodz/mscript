use crate::variables::Primitive::{self, *};
use crate::*;
use anyhow::{bail, Result};

impl std::ops::Add for Primitive {
    type Output = Result<Primitive>;

    fn add(self, rhs: Self) -> Self::Output {
        let (t1, t2) = (&self, &rhs);
        let math = apply_math_bin_op_if_applicable!(t1 + t2);

        if let Some(result) = math {
            return Ok(result);
        }

        Ok(match (self, rhs) {
            (Str(x), Str(y)) => string!(x + &y),
            (Str(x), y) => string!(x + &y.to_string()),
            (x, Str(y)) => string!(x.to_string() + &y),
            (Vector(x), Vector(y)) => {
                let mut x = arc_to_ref(&x).clone();

                x.extend_from_slice(&y);
                vector!(raw x)
            }
            _ => bail!("valid ops: number + number, str + any, any + str, vec + vec"),
        })
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn five_plus_five() {
        let result = int!(5) + float!(5.0);
        assert_eq!(result.unwrap(), float!(10.0))
    }

    #[test]
    fn two_plus_two_hundred() {
        let result = bigint!(2) + int!(200);
        assert_eq!(result.unwrap(), bigint!(202))
    }

    #[test]
    fn one_plus_negative_one_thousand() {
        let result = bigint!(1) + float!(-1_000.0);
        assert_eq!(result.unwrap(), float!(-999.0))
    }

    #[test]
    #[should_panic]
    fn integer_limit_plus_one() {
        let _result = int!(i32::MAX) + int!(1);
    }
}
