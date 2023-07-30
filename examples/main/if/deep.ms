if true {
	if true {
		if true {
			if true {
				if true {
					print "hi"
				}
			}
		}
	}
}

a = fn() -> fn() -> fn() -> fn() {
	return fn() -> fn() -> fn() {
		return fn() -> fn() {
			return fn() {
				print "wow, this is so deep"
			}
		}
	}
}

x = fn() {
	print "hi"
	self()
}

(((a())())())()