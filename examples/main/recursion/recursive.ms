c = 0

r = fn() {
	print c += 1
	self()
}
r()