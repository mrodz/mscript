use anyhow::{bail, Result};

use crate::{bytecode::variables::Primitive::{self, *}, apply_math_bin_op_if_applicable};

impl std::ops::Rem for Primitive {
	type Output = Result<Primitive>;
	
	fn rem(self, rhs: Self) -> Self::Output {
		match rhs {
			Int(0) | BigInt(0) => bail!("% by 0"),
			Float(f) if f == 0.0 => bail!("% by 0"),
			_ => ()
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