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

			fn noargs(self) {}
		}

		sqrtNeg1 = ImaginaryNumber(0f, 1f)
print sqrtNeg1.noargs