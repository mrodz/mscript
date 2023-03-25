use crate::bytecode::variables::buckets::{Int, Float, BigInt, Byte};

impl PartialEq<Int> for Float {
    fn eq(&self, rhs: &Int) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as f64
	}
}

impl PartialEq<BigInt> for Float {
    fn eq(&self, rhs: &BigInt) -> bool {
		let val = self.0;
		let other = rhs.0;

		val as i128 == other
	}
}

impl PartialEq<Byte> for Float {
	fn eq(&self, rhs: &Byte) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as f64
	}
}