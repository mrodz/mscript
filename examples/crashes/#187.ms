###
# bug 1
class Person {
    name: str
    friends: [Self...]

    constructor(self, name: str) {
        self.name = name
        self.friends = []
    }

    fn add_friend(self, friend: Self) {
        self.friends.push(friend)
    }
}
###



# bug 2
class Person {
    #name: str
    #friends: [Self...]

    constructor(self) {
        #self.name = name
        #self.friends = []
    }
    
    fn add_friend(self, friend: Self) {
        #self.friends.push(friend)
    }
}

class N {
	fn x(self) {
		(Person()).add_friend(self)
	}
}


(N()).x()
###