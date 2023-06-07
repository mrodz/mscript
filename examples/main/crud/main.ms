a = 5

increment_a = fn() -> fn() {
	return fn() {
		modify a = a + 5
	}
}

print a
increment_a()()
print a
increment_a()()
print a
