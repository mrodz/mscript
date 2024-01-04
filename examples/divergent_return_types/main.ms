test = fn() -> int {
	if true {
		if true {
			return 20
		} else {
			return 9
		}
	}

	return -1
}

wont_compile = fn() -> int {
	if false {
		return "this will not compile because the function is supposed to return an int"
	} else {
		return 5
	}
}

wont_compile_2 = fn() -> int {
	if false {
		return 5
	}
}

wont_compile_3 = fn() -> int {
	print "This won't compile, even though *theoretically* it never diverges. For now, this is a feature and not a bug."
	if true {
		return 10
	}
}

x = test()
print x

return 10