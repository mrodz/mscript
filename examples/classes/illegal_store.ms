class A {
	fn function(self) {
		print "hello"
	}
}

a = A()

print a

take_void_function = fn(input: fn()) {
	print input
}

take_void_function(fn() {})