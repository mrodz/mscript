x = 5



adder = fn(b: int) -> (fn(int) -> int) {
	return fn(input: int) -> int {
		return input + b
	}
}

add_twenty = adder(20)
print add_twenty(5)

