i = 0

is_even = fn(input: int) -> bool {
	return input % 2 == 0
}

while i < 10 {
	i = i + 1

	if is_even(i) {
		continue
	}

	print "cool whip " + i

}

print "done\n"

i = 0

while true {
	i = i + 1

	print i

	if i == 5 {
		print "breaking..."
		break
	}
}

print "done v2!"