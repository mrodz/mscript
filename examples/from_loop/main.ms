n = 10

print "[🚀 1/4] Loop with step and control flow:"

from 0 through 100 step 5, n {
	print n
	if n == 50 {
		print "I reached fifty!!"
		break
	}
}

print "Afterwards, n is: " + n

number = 10

print "\n" * 3 + "[🚀 2/4] Inclusive with no control flow:"
from 0 through 3, number {
    print number
}

print "Afterwards, number is = " + number

print "\n" * 3 + "[🚀 3/4] Through loop with floating points... it's messy, but that's floating point for ya"
from 1 to 1.5 step 0.1, number {
	print number
}
print "Done!"

print "\n" * 3 + "[🚀 4/4] Nested Loops:"

from 0 to 3, i {
	from 0 to 3, j {
		print "(" + i + ", " + j + ")"

		if i == 1 && j == 1 {
			print "CENTER!!! skipping the next one"
			break
		}

	}
}

print "Cool beans 😎"