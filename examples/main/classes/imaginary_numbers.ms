class ImaginaryNumber {
	real: float
	imaginary: float

	constructor(self, real: float, imaginary: float) {
		self.real = real
		self.imaginary = imaginary
	}

	fn to_string(self) -> str {
		return "" + self.real + " + " + self.imaginary + "i"
	}
}

sqrtNeg1 = ImaginaryNumber(0f, 1f)
assert sqrtNeg1.to_string() == "0 + 1i"

print sqrtNeg1.to_string()