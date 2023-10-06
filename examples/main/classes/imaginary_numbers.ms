
class ImaginaryNumber {
	real: float
	imaginary: float

	constructor(self, real: float, imaginary: float) {
		self.real = real
		self.imaginary = imaginary
	}

	fn noargs(self) {}
}

sqrtNeg1 = ImaginaryNumber(0f, 1f)
sqrtNeg1.noargs()