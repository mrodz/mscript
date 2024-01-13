a = fn() -> int { return 5 }

# print [1, 2, 3].length

const x = [5]

take_ints = fn(in: [int]) {
	print in
	print in.len()
}

take_ints(x)

x: [int...] = [1, 2, 3, 4, 5]

x.reverse()

print x
print x.inner_capacity()

x.ensure_inner_capacity(100)

print x.inner_capacity()

print x