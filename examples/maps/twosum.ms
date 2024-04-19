###
two_sum = fn(input: [int...], target: int) -> [int, int]? {
	values_needed_for_sum = map[int, int]

	from 0 to input.len(), i {
		corresponding_index = values_needed_for_sum[input[i]]
		if corresponding_index != nil {
			return [get corresponding_index, i]
		} else {
			values_needed_for_sum[target - input[i]] = i
		}
	}

	return nil
}

print two_sum([2,7,11,15], 9)

###

class FactorialPair {
			number: int
			factorial: bigint

			constructor(self, number: int) {
				self.number = number
				self.set_factorial()
			}

			fn set_factorial(self) {
				result: bigint = B1

				from 0x1 through self.number, n {
					result = result * n
				}

				self.factorial = result
			}

			fn to_string(self) -> str {
				return "Factorial of " + self.number + " is " + self.factorial
			}
		}

		fact6 = FactorialPair(6)
		fact1 = FactorialPair(1)
		fact32 = FactorialPair(32)

		assert fact1.to_string() == "Factorial of 1 is 1"
		assert fact6.to_string() == "Factorial of 6 is 720"
		assert fact32.to_string() == "Factorial of 32 is 263130836933693530167218012160000000"