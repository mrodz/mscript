use crate::variables::Primitive::*;
use crate::*;
use anyhow::{bail, Result};

impl std::ops::Add for Primitive {
    type Output = Result<Primitive>;
    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl std::ops::Add for &Primitive {
    type Output = Result<Primitive>;

    fn add(self, rhs: Self) -> Self::Output {
        let (t1, t2) = (&self, &rhs);
        let math = apply_math_bin_op_if_applicable!(t1 + t2);

        if let Some(result) = math {
            return Ok(result);
        }

        Ok(match (self, rhs) {
            (Str(x), Str(y)) => string!(x.to_owned() + y),
            (Str(x), y) => string!(x.to_owned() + &y.to_string()),
            (x, Str(y)) => string!(x.to_string() + y),
            (Vector(x), Vector(y)) => {
                let mut x = Vec::clone(x.0.borrow().as_ref());

                x.extend_from_slice(y.0.borrow().as_ref());
                vector!(raw x)
            }
            (x, y) => bail!("<{:?} + {:?}> is invalid. (valid ops are: <num + num>, <str + any>, <any + str>, <vec + vec>", x.ty(), y.ty()),
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
