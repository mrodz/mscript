i = 0

random_call = fn() -> bool {
	return i == 5
}

while i < 10 && !random_call() {
	print i
	i = i + 1
}