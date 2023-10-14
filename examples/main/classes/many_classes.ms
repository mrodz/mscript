class D {
	fn get_value(self) -> int {
		return 42
	}
}

class C {
	fn d(self) -> D {
		return D()
	}
}

class B {
	fn c(self) -> C {
		return C()
	}
}

class A {
	fn b(self) -> B {
		return B()
	}
}

print (A()).b().c().d().get_value()