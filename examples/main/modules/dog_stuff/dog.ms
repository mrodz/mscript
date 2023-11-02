class Dog {
	name: str
	breed: str

	constructor(self, name: str, breed: str) {
		self.name = name
		self.breed = breed
	}

	fn to_string(self) -> str {
		return "Dog { name: " + self.name + ", breed: " + self.breed + "}"
	}
}

my_dog_name = "Scout"

export { Dog, my_dog_name }