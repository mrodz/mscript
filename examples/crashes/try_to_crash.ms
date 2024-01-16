class Dog {
	name: str
	constructor(self, name: str) {
		self.name = name
	}
	fn to_str(self) -> str {
		return "Dog named " + self.name
	}
}

class Cat {
	name: str
	breed: str
	constructor(self, name: str, breed: str) {
		self.name = name
		self.breed = breed
	}
	fn to_str(self) -> str {
		return "Cat named " + self.name + " is a " + self.breed
	}
}

dogs: [Dog...] = [Dog("Old Yeller"), Dog("Air Bud"), Dog("Odie")]

dogs.push(Dog("Scout"))
dogs.push(Dog("Muna"))

assert dogs.len() == 5
assert (dogs[3]).name == "Scout"

dogs_str: [str...] = []
from 0 to dogs.len(), i {
	dogs_str.push((dogs[i]).to_str() + ": " + typeof dogs[i])
}

assert dogs_str == ["Dog named Old Yeller: Dog", "Dog named Air Bud: Dog", "Dog named Odie: Dog", "Dog named Scout: Dog", "Dog named Muna: Dog"]

dogs.reverse()

maybe_dogs = dogs.map(fn(x: Dog) -> Dog? { return x })

assert typeof maybe_dogs == "[Dog?...]"
assert dogs == maybe_dogs

maybe_dogs_str: [str...] = []

from 0 to maybe_dogs.len(), i {
	maybe_dogs_str.push((maybe_dogs[i]).to_str() + ": " + typeof maybe_dogs[i])
}

assert maybe_dogs_str == ["Dog named Muna: Dog?", "Dog named Scout: Dog?", "Dog named Odie: Dog?", "Dog named Air Bud: Dog?", "Dog named Old Yeller: Dog?"]

cats = maybe_dogs.map(fn(x: Dog?) -> Cat { return Cat((get x).name, "tabby") })

cats_str: [str...] = []
from 0 to cats.len(), i {
	cats_str.push((cats[i]).to_str() + ": " + typeof cats[i])
}

assert cats_str == ["Cat named Muna is a tabby: Cat", "Cat named Scout is a tabby: Cat", "Cat named Odie is a tabby: Cat", "Cat named Air Bud is a tabby: Cat", "Cat named Old Yeller is a tabby: Cat"]