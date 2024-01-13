INSTANCE_COUNTER = 0

class A {
	constructor(self) {
		INSTANCE_COUNTER += 1

		assert INSTANCE_COUNTER <= 2
	}

	fn assoc(self) -> Self? {
		return self
	}
}

a = A()

type B A
type C B

b: B = A()

take_a = fn(in: C) {
	print in
}

print a
print a.assoc()
take_a(a)
take_a(b or A())
take_a((a.assoc()) or A())

class X {
	x: Self
	constructor(self) {
		self.x = self
	}
}

x = X()

print x
print x.x.x.x.x