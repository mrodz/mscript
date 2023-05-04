use anyhow::{bail, Result};

use crate::{bytecode::variables::Primitive::{self, *}, int, float, bigint, apply_math_bin_op_if_applicable, string, vector};

impl std::ops::Add for Primitive {
	type Output = Result<Primitive>;
	
	fn add(self, rhs: Self) -> Self::Output {
		let (t1, t2) = (&self, &rhs);
		let math = apply_math_bin_op_if_applicable!(t1 + t2);

		if let Some(result) = math {
			return Ok(result)
		}

		Ok(match (self, rhs) {
			(Str(x), Str(y)) => string!(x + &y),
			(Str(x), y) => string!(x + &y.to_string()),
			(x, Str(y)) => string!(x.to_string() + &y),
			(Vector(x), Vector(y)) => {
				let mut x = x.clone();
				x.extend_from_slice(&y);
				vector!(raw x)
			},
			_ => bail!("valid ops: number + number, str + any, any + str, vec + vec")
		})
	}
}