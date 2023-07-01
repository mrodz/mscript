print "Numeric Comparisons:"
print 5 > 3
print 5 < 3
print 5 <= 5
print 5 <= 4
print 5 >= 5
print 5 >= 6
print 5 == 5
print 5 == 6
print 5 != 6
print 5 != 5
print "6" == "6"

print "\n" * 3 + "Boolean Comparisons:"
print true && true
print true && false
print true || false
print false || false
print false ^ true
print false ^ false
print !false
print !true
print !(true && false)

print "\n" * 3 + "Lazy + Eager Comparisons:"

a = fn() -> bool {
	print "a"
	return true
}

b = fn() -> bool {
	print "b"
	return false
}

print a() && b()
print b() && a()
print a() || b()
print b() || a()
print a() ^ b()