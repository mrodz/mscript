use crate::bytecode::variables::buckets::{Byte, BigInt, Float, Int};

impl PartialEq<Float> for Byte {
    fn eq(&self, rhs: &Float) -> bool {
        let val = self.0;
        let other = rhs.0;

        val as f64 == other
    }
}

impl PartialEq<Int> for Byte {
    fn eq(&self, rhs: &Int) -> bool {
        let val = self.0;
        let other = rhs.0;

        val as i32 == other
    }
}

impl PartialEq<BigInt> for Byte {
    fn eq(&self, rhs: &BigInt) -> bool {
        let val = self.0;
        let other = rhs.0;

        val as i128 == other
    }
}
