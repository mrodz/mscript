x = 5

a = fn(c: int) {
	y = 3
	b = fn() {
		print y + x * c
	}
	b()
}

a(100)

adder = fn(b: int) -> (fn(int) -> int) {
	return fn(input: int) -> int {
		return input + b
	}
}

add_twenty = adder(20)
print add_twenty(5)
