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

factorial_through_loop = fn(input: int) -> int {
	result = 1
	from 1 through input, n {
		result = result * n
	}
	return result
}

print factorial_through_loop(5)

factorial_callback = fn(input: int) -> (fn() -> int) {
	return fn() -> int {
		result = 1
		from 1 through input, n {
			result = result * n
		}
		return result
	}
}

factorial_fn = factorial_callback(5)
print factorial_fn()
factorial_fn2 = factorial_callback(6)
print factorial_fn2()
print factorial_fn()