class ComplexNumber {
	real: float
	imaginary: float

	constructor(self, r: float, i: float) {
		self.real = r
		self.imaginary = i
	}
}

class A {
	x: int
}

print A()

sqrt_neg_1: ComplexNumber = ComplexNumber(0f, 1.0)

print sqrt_neg_1

class B {
	b_field: fn()

	constructor(self) {
		self.b_field = fn() {
			print "B field"
		}
	}

	fn b_method(self) {
		print "B method"
	}
}

b = B()
b.b_method()
b.b_field()