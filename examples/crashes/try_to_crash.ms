outside = 5

a = fn() {
	outside
}

b = a

print a.is_closure()
print b.is_closure()

print a.to_str()
print b.to_str()

print typeof true
print typeof true.to_str()