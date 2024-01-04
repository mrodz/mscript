truthy = fn() -> bool {
	return true
}

falsey = fn() -> bool {
	return false
}

if false {
	print "unreachable"
} else if falsey() {
	print "wooh! this message is a secret."
} else if truthy() {
	print "boo-yah!!"
} else {
	print "never"
}