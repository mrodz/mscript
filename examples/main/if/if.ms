truthy = fn() -> bool {
	return true
}

falsey = fn() -> bool {
	return false
}

if falsey() {
	print "hi"
} else if true {
	print "awww"
}