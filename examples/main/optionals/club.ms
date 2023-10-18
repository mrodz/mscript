class Person {
	name: str
	age: int

	constructor(self, name: str, age: int) {
		self.name = name
		self.age = age
	}
}

new_club_member = fn(name: str, age: int) -> Person? {
	if age >= 21 {
		return Person(name, age)
	}
	
	return nil
}

give_drink = fn(age: int) -> str? {
	if age < 21 {
		return nil
	}

	return "Booze"
}

take_optional = fn(input: str?) {
	print input
}

triple_it = fn(input: str) -> str {
	return input * 3
}

take_optional("cool")
take_optional(nil)

take_optional(triple_it("\n"))

print new_club_member("Mateo", 16)
print new_club_member("Devin", 22)

# print triple_it(give_drink(21))