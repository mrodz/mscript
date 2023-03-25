use crate::bytecode::variables::buckets::{Int, Float, BigInt, Byte};

impl PartialEq<Int> for BigInt {
    fn eq(&self, rhs: &Int) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as i128
	}
}

impl PartialEq<Float> for BigInt {
    fn eq(&self, rhs: &Float) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as i128
	}
}

impl PartialEq<Byte> for BigInt {
	fn eq(&self, rhs: &Byte) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as i128
	}
}