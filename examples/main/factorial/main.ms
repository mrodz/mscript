factorial_impl = fn(input: int, result: int, counter: int) -> int {
	if counter > input {
		return result
	}

	result = result * counter

	return self(input, result, counter + 1)
}

factorial = fn(input: int) -> int {
	return factorial_impl(input, 1, 1)
}

print factorial(5)