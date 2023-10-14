class IntPromise {
	resolve: fn(int)
	reject: fn(str)

	constructor(self, resolve: fn(int), reject: fn(str)) {
		self.resolve = resolve
		self.reject = reject
	}
}