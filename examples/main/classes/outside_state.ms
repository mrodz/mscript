MESSAGE = "Hello "

class Greeter {
	fn noargs(self) {
		x = MESSAGE
		print x
	}
}

hi_mateo = Greeter("Mateo")

hi_mateo.noargs()