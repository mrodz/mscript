use crate::bytecode::variables::buckets::{Int, Float, BigInt, Byte};

impl PartialEq<Float> for Int {
    fn eq(&self, rhs: &Float) -> bool {
		let val = self.0;
		let other = rhs.0;

		val as f64 == other
	}
}

impl PartialEq<BigInt> for Int {
    fn eq(&self, rhs: &BigInt) -> bool {
		let val = self.0;
		let other = rhs.0;

		val as i128 == other
	}
}

impl PartialEq<Byte> for Int {
	fn eq(&self, rhs: &Byte) -> bool {
		let val = self.0;
		let other = rhs.0;

		val == other as i32
	}
}