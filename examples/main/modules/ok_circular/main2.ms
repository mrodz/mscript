import lib

class D {
	fn drawPixel2(self) -> str {
		lib.drawPixel(40, 20)
		return "BYEEEEE"
	}
}

print "\n\nBegin Test:\n"
print (D()).drawPixel2()

assoc_fn = lib.drawPixel
assoc_fn(6, 9)

lib.drawPixel(55, 55)