instances = 0

class A {
    constructor(self) {
        modify instances = instances + 1
    }

    fn get_instances(self) -> int {
        return instances
    } 
}

first = A()
second = A()
third = A()

print first
print second
print third

print first.get_instances()