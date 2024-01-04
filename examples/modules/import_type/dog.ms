export class Dog {
	constructor(self, name: str) {
		self.name = name
	}
	name: str
	fn to_string(self) -> str {
		return "Dog with name " + self.name
	}
}

x: Dog = Dog("Cool :)")

export type Floof Dog