time_it = fn(callback)
	result = fn(args)
		start = os::time()
		callback(args)
		end = os::time()

		time = end - start
		print("it took {time} ms!")

	return result