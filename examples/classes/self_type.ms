class A {
	state: int

	constructor(self, state: int) {
		self.state = state
	}

	fn clone_plus(self, add: Self) -> Self {
		return Self(self.state + add.state)
	}

	fn to_string(self) -> str {
		return "A("+self.state+")"
	}
}

a = A(5)
b = A(7)
c = a.clone_plus(b)

print a.to_string()
print b.to_string()
print c.to_string()