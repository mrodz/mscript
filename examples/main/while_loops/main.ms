i = 0

random_call = fn() -> bool {
	temp = i == 20
	return temp
}

while i < 9 && !random_call() {
	print i
	i = i + 1
}

print "At the end, I = " + i