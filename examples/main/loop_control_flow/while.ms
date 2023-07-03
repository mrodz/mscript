i = 0

is_even = fn(input: int) -> bool {
	return input % 2 == 0
}

while i < 10 {
	if i % 2 == 0 {
		print "" + i + " is even!"
	}

	print "cool whip " + i

	i = i + 1
}

print "done"
