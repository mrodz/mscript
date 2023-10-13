

class Dog {
    name: str
    
    constructor(self, name: str) {
        self.name = name
    }

    fn play(self, other: Self) -> str {
        return self.name + " is playing with " + other.name
    }

    fn strit(self) -> str {
        return "DOG " + self.name
    }

    fn birds_and_bees(self, other: Self) -> Self {
        return Self("child")
	}
}

x = Dog("Scout")
y = Dog("Muna")
	
print x.play(y)

print x.strit()
print y.strit()

print x.strit()