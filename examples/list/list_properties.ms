square_it = fn(in: int) -> int {
	return in * in
}

only_even = fn(in: int) -> bool {
	return in % 2 == 0
}

x = [3, 4, 5].map(square_it).filter(only_even)

print x
print x.len()
print x.inner_capacity()
x.push(100)
x.push(9999999)
print x
print x.remove(1)
print x
x.push(5)

type numberz int
AAA: numberz = 69
x.push(AAA)

print x
print typeof x
x = x.map(fn(in: int) -> str {
	return in + "!"
})

x = x.join(["hello", "world"])

print x

print x.index_of("5!")
print x.index_of("5")