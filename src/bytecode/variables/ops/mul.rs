use anyhow::{bail, Result};

use crate::{bytecode::variables::Primitive::{self, *}, int, float, bigint, apply_math_bin_op_if_applicable, string, vector};

impl std::ops::Mul for Primitive {
	type Output = Result<Primitive>;
	
	fn mul(self, rhs: Self) -> Self::Output {
		let (t1, t2) = (&self, &rhs);
		let math = apply_math_bin_op_if_applicable!(t1 * t2);

		if let Some(result) = math {
			return Ok(result)
		}

		/// we cannot use built-in repeat, since Primitive does not implement Copy.
		fn repeat_vec(original: &Vec<Primitive>, new_size: usize) -> Result<Vec<Primitive>> {
			let len = original.len();
			let Some(new_size) = len.checked_mul(new_size) else {
				bail!("new size is too large")
			};
			let mut result = Vec::with_capacity(new_size);
			for i in 0..new_size {
				result[i] = original[i % len].clone()
			}

			Ok(result)
		}

		Ok(match (self, rhs) {
			(Str(x), Int(y)) => string!(x.repeat(y.try_into()?)),
			(Str(x), BigInt(y)) => string!(x.repeat(y.try_into()?)),
			(Vector(ref x), Int(y)) => vector!(raw repeat_vec(x, y.try_into()?)?),
			(Vector(ref x), BigInt(y)) => vector!(raw repeat_vec(x, y.try_into()?)?),
			_ => bail!("valid ops: number * number, str * number, vec * number")
		})
	}
}