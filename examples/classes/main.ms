class Person {
	first_name: str
	last_name: str
	age: int
	
	constructor(self, first_name: str, last_name: str, age: int) {
		self.first_name = first_name
		self.last_name = last_name
		self.age = 16
	}
	
	fn grow_up(self) {
		self.age = self.age + 1
	}	
	
	fn full_name(self) -> str {
		return self.first_name + " " + self.last_name
	}
}
	
me = Person("John", "Doe", 47)
	
print me.age
me.grow_up()
print me.age
	
print me.full_name()
	