class Dog {
	name: str
		
	constructor(self, name: str) {
		self.name = name
	}
	
	fn play(self, other: Self) -> str {
		return self.name + " is playing with " + other.name
	}
	
	fn birds_and_bees(self, other: Self) -> Self {
		baby_name = self.name + " & " + other.name + "'s Baby"
		return Self(baby_name)
	}
}
	
x = Dog("Scout")
y = Dog("Muna")
		
assert x.play(y) == "Scout is playing with Muna"
	
pup = x.birds_and_bees(y)
	
assert pup.name == "Scout & Muna's Baby"