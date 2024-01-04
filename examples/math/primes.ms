l_range = 1
u_range = 10_000
print "Prime numbers between " + l_range + " and " + u_range + " are:"

num_primes = 0

from l_range through u_range, num {
	# all prime numbers are greater than 1
	if num > 1 {
		i = 0
		from 2 to num, i {
			if num % i == 0 {
				break
			}
		}

		# check if iteration has completed by comparing index `i` to end value
		if i == num {
			num_primes += 1
			print num
		}
	}
}

print "In total, found: " + num_primes + " prime numbers"