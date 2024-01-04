a = 5

increment_a = fn() {
	modify a = a + 5
}

print a
increment_a()
print a
increment_a()
print a

test2 = fn() -> int {
	x = 10
	y = 30
	b = 50
	return x + y + b + a
}

print test2()