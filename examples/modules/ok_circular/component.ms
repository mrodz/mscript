import lib

export class Button {
	x: int
	y: int
	
	constructor(self, x: int, y: int) {
		self.x = x
		self.y = y
	}

	fn paint(self) {
		lib.drawPixel(self.x, self.y)
	}
}