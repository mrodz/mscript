class Person {
  first_name: str
  last_name: str
  age: int

  constructor(self, first_name: str, last_name: str, age: int) {
    self.first_name = first_name
    self.last_name = last_name
    self.age = age
  }

  fn grow_up(self) {
    self.age += 1
  }

  fn get_full_name(self) -> str {
    return self.first_name + " " + self.last_name + " is " + self.age + " years old"
  }
}

mateo = Person("John", "Doe", 99)

print mateo.get_full_name()

mateo.grow_up()

print mateo.get_full_name()
