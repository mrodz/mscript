

factorial = fn(input: int) -> bigint {
	result = B1

	from 1 through input, i {
		result *= i
	}

	return result
}

print factorial(5)

maybe: int? = nil
print maybe + 1