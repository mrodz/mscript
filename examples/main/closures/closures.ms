x = 5

a = fn(c: int) {
	y = 3
	b = fn() {
		print y + x * c
	}
	b()
}

a(100)