export class Node {
	value: int
	next: Self?

	constructor(self, value: int, next: Self?) {
		self.value = value
		self.next = next
	}

	fn add(self, node: Self) {
		if self.next == nil {
			self.next = node
		} else {
			self.next.add(node)
		}
	}
}