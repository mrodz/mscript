class A {
    value: int
    constructor(self) {
        self.value = 5
    }
    fn make_new_a(self) -> Self {
        new: Self = Self()
        new.value = 10
        return new
    } 
}