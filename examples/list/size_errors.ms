two_numbers: [int, int] = [1, 2]

bad_four_numbers: [int, int, int, int] = [1, 2, 3, 4]

print_list = fn(input: [int, int, int]) {
	print input[0]
	print input[1]
	print input[2]
}

print_list([100, 200, 300])

print_many = fn(input: [int...], len: int) {
	from 0 to len, i {
		print input[i]
	}
}

print_many([1, 2, 3, 4, 5], 5)

1 + 2
print 3