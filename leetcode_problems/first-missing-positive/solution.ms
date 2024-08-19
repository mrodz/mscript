###
Given an unsorted integer array nums. Return the smallest positive integer that is not present in nums.

You must implement an algorithm that runs in O(n) time and uses O(1) auxiliary space.

 

Example 1:

Input: nums = [1,2,0]
Output: 3
Explanation: The numbers in the range [1,2] are all in the array.
Example 2:

Input: nums = [3,4,-1,1]
Output: 2
Explanation: 1 is in the array but 2 is missing.
Example 3:

Input: nums = [7,8,9,11,12]
Output: 1
Explanation: The smallest positive integer 1 is missing.
 

Constraints:

1 <= nums.length <= 105
-231 <= nums[i] <= 231 - 1
###

first_missing_positive = fn(nums: [int...]) -> int {
	from 0 to nums.len(), i {
		if nums[i] <= 0 {
			nums[i] = nums.len() + 1
		}
	}

	from 0 to nums.len(), i {
		num_abs = (nums[i]).abs()

		if num_abs - 1 < nums.len() && nums[num_abs - 1] > 0 {
			(nums[num_abs - 1]) *= -1
		}
	}

	from 0 to nums.len(), i {
		if nums[i] > 0 {
			return i + 1
		}
	}

	return nums.len() + 1
}

test_cases = map[[int...], int] {
	[1,2,0]: 3,
	[3,4,-1,1]: 2,
	[7,8,9,11,12]: 1,
	[50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]: 51,
	[1,2,6,3,5,4]: 7,
}.pairs()

print "🔵 Starting testing suite"

from 0 to test_cases.len(), i {
	[input, expected] = test_cases[i]

	output = first_missing_positive(input)
	
	if output != expected {
		print "🛑 Error in test suite: expected " + expected.to_str() + ", found: " + output.to_str()
		assert false
	}

	print "    ☐ Passed test #" + i
}

print "✅ Passed " + test_cases.len() + " test cases"