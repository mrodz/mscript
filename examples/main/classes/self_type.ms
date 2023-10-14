class A {
	state: int

	constructor(self, state: int) {
		self.state = state
	}

	fn clone_plus(self, add: int) -> Self {
		return Self(self.state + add)
	}

	fn clone_plus(self, add: Self) -> Self {
		print self.state
		return Self(1)
	}

	fn to_string(self) -> str {
		return "A("+self.state+")"
	}
}

a = A(5)
