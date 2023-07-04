factorial_impl = fn(input: int, result: int, counter: int) -> int {
	if counter > input {
		return result
	}

	result = result * counter

	return self(input, result, counter + 1)
}

factorial_recursion = fn(input: int) -> int {
	return factorial_impl(input, 1, 1)
}

print factorial_recursion(5)

factorial_while_loop = fn(input: int) -> int {
	result = 1
	counter = 1
	while counter <= input {
		result = result * counter
		counter = counter + 1
	}
	return result
}

print factorial_while_loop(5)