use anyhow::{bail, Result};

use crate::{bytecode::variables::Primitive, apply_math_bin_op_if_applicable};

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