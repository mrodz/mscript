###
class ConditionFactory {
        valid_inputs: [[int, str]...]

        constructor(self) {
                self.valid_inputs = []
        }

        fn with_input(self, number: int, repr: str) -> Self {
                self.valid_inputs.push([number, repr])
                return self
        }

        fn len(self) -> int {
                return self.valid_inputs.len()
        }

        fn nth(self, index: int) -> [int, str] {
                return (self.valid_inputs)[index]
        }
}

class FizzBuzzFactory {
        conditions: [ConditionFactory...]

        constructor(self) {
                self.conditions = []
        }

        fn with_condition(self, condition_factory: ConditionFactory) -> Self {
                self.conditions.push(condition_factory)
                return self
        }

        fn build(self) -> (fn(int) -> str) {
                return fn(input: int) -> str {
                        result = ""
                        from 0 to self.conditions.len(), i {
                                conditions = (self.conditions)[i]

                                from 0 to conditions.len(), j {
                                        [number, repr] = conditions.nth(j)

                                        if input % number == 0 {
                                                result += repr
                                        }
                                }
                        }
                        return result
                }
        }
}

fizzbuzz = (FizzBuzzFactory())
                .with_condition(
                        (ConditionFactory())
                                .with_input(3, "Fizz")
                )
                .with_condition(
                        (ConditionFactory())
                                .with_input(5, "Buzz")
                )
                .build()

print fizzbuzz(15)
###

class Foo {
        fn bar(self) -> fn() {
                return fn() {
                        print self
                }
        }
}

foo = Foo()
x = foo.bar()
x()