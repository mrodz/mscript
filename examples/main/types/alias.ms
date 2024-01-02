type Miles int

marathon: Miles = 6

print marathon

export x: fn() = fn() {
	type Meters int

	a: Meters = 10
	b: Miles = 20
	c: int = a
	d: Miles = a

	print a
	print b
	print c
	print d
}