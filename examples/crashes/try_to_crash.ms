class A {
	field: [Self?...]?

	fn set_field(self, field: [Self...]) {
		self.field = field
	}

	fn to_str(self) -> str {
		return "A { field: " + self.field.to_str() + " }"
	}

	fn equ(self, rhs: Self) -> bool {
		return self.field == rhs.field
	}
}

a = A()

type B A
b: B? = A()

a.set_field([a, b or a, A()])

assert a.equ(get (get a.field)[0])
assert a is (get a.field)[0]
assert a is get (get a.field)[0]