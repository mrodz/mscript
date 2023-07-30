goodbye = fn() {
	print "bye!"
}

hello = fn() {
	print "wassup!"
	goodbye()
	self()
}

hello()