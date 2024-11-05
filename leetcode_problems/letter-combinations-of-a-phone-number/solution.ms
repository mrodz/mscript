###
Given a string containing digits from 2-9 inclusive, return all 
possible letter combinations that the number could represent. 

Return the answer in any order.

A mapping of digits to letters (just like on the telephone buttons) 
is given below. Note that 1 does not map to any letters.
###

digit_to_characters = map[int, str] {
	2: "abc",
	3: "def",
	4: "ghi",
	5: "jkl",
	6: "mno",
	7: "pqrs",
	8: "tuv",
	9: "wxyz"
}

class Solution {
	fn letterCombinations(self, digits: str) -> [str...] {
		if digits.len() == 0 {
			return []
		}

		return self.recursiveSolution(digits, "")
	}

	fn recursiveSolution(self, digits: str, prefix: str) -> [str...] {
		if digits.len() == 0 {
			return [prefix]
		}

		digit = get (digits[0]).parse_int()

		permutations = (get digit_to_characters[digit]).chars()

		result: [str...] = []

		from 0 to permutations.len(), i {
			result.join(self.recursiveSolution(digits.substring(1, digits.len()), prefix + permutations[i]))
		}

		return result
	}
}

inputs: [str...] = ["2345", "35", "7", "89925"]

solution = Solution()

from 0 to inputs.len(), i {
	print solution.letterCombinations(inputs[i])
}