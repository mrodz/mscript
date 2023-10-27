fizz_buzz = fn(input: int) -> str? {
	result: str? = nil

	if input % 3 == 0 {
		result = "Fizz"
	}

	if input % 5 == 0 {
		preappend = get result or ""
		result = preappend + "Buzz"
	}

	return result	
}

from 1 to 10, i {
	if label ?= fizz_buzz(i) {
		print label
	} else {
		print i
	}
}