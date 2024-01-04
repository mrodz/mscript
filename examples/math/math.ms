add_five: fn(int) = fn(x: int) {
	x = x + 5
	print x
}

print "hello" * 10
print 0b101 + 0b010

shadow = add_five

shadow(20 * 50)

my_name: str = "Mateo" * 10
print my_name

print 1 + 2 * 3
print ((1 + 2) * 3)
print (-(1 + 2) * 3)
print ((1 + 2) * 3)

print "\n" * 2

print "Wow! I love " + "MScript" 

print 3 * ("" + -(1 + 2) * 3)

give_five = fn() -> int {
	return 5
}


idk: int = give_five()


i: str = "5"

give_five()
