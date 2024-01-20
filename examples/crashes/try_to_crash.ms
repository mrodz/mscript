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
b = A()

a.set_field([a, b, A()])

print a.equ(get (get a.field)[0])
print a is (get a.field)[0]
print a is get (get a.field)[0]