print "lib.ms"

export const version: str = "19.5.2"

export const drawPixel: fn(int, int) = fn(x: int, y: int) {
	print "Drawing pixel at: (" + x + ", " + y + ")"
}