import Node from node

class LinkedList {
	inner: Node?
	tail: Node?

	constructor(self, inner: Node?) {
		self.inner = inner
	}

	fn add(self, number: int) {
		inner: Node? = nil
		if inner ?= self.inner {
			inner.add(Node(number, nil))
		} else {
			self.inner = Node(number, nil)
		}
	}

	fn to_string(self) -> str {
		result = "List: "

		next: Node? = nil
		
		if next ?= self.inner {
			result += next.value
		} else {
			return "<empty list>"
		}

		while next ?= next.next {
			result += ", " + next.value
		}

		return result
	}
}

export new_empty: fn() -> LinkedList = fn() -> LinkedList {
	return LinkedList(nil)
}