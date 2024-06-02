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
                all_conditions = self.conditions
                return fn(input: int) -> str {
                        result = ""
                        from 0 to all_conditions.len(), i {
                                conditions = all_conditions[i]

                                from 0 to conditions.len(), j {
                                        [number, repr] = conditions.nth(j)

                                        if input % number == 0 {
                                                result += repr
                                        }
                                }
                        }
                        if result.len() == 0 {
                                return input.to_str()
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
                                .with_input(7, "Bazz")
                )
                .build()

frequencies = map[str, int] {
        "Number": 0,
}

from 1 through 105, i {
        message = fizzbuzz(i)

        if message.parse_int() == nil {
                frequencies[message] = ((frequencies[message]) or 0) + 1
        } else {
                frequencies["Number"] = frequencies["Number"] + 1
        }
}

assert frequencies["Fizz"] == 24
assert frequencies["Buzz"] == 12
assert frequencies["Bazz"] == 8
assert frequencies["FizzBuzz"] == 6
assert frequencies["FizzBazz"] == 4
assert frequencies["BuzzBazz"] == 2
assert frequencies["FizzBuzzBazz"] == 1
assert frequencies["Number"] == 48