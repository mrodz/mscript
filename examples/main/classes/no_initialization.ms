class ComplexNumber {
	real: float
	imaginary: float

	constructor(self) {

	}
}

class A {
	x: int
}

print A()

sqrt_neg_1: ComplexNumber = ComplexNumber(0, 1)

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