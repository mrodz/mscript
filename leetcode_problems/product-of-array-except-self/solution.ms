###
Given an integer array nums, return an array answer such that answer[i] is equal to the product of all the elements of nums except nums[i].

The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.

You must write an algorithm that runs in O(n) time and without using the division operation.

 

Example 1:

Input: nums = [1,2,3,4]
Output: [24,12,8,6]
Example 2:

Input: nums = [-1,1,0,-3,3]
Output: [0,0,9,0,0]
 

Constraints:

2 <= nums.length <= 105
-30 <= nums[i] <= 30
The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
 

Follow up: Can you solve the problem in O(1) extra space complexity? (The output array does not count as extra space for space complexity analysis.)
###

product_of_array_except_self = fn(nums: [int...]) -> [int...] {
	result: [int...] = []
	result.ensure_inner_capacity(nums.len())

	left_prefixes: [int?...] = [nil, nums[0]]

	# walk from left to right
	from 1 to nums.len() - 1, i {
		left_prefixes.push(left_prefixes[i] * nums[i])
	}

	right_prefixes: [int?...] = [nil, nums[nums.len() - 1]]

	# walk from right to left
	from 1 to nums.len() - 1, i {
		right_prefixes.push(right_prefixes[i] * nums[nums.len() - i - 1])
	}

	# aggregate
	from 0 to nums.len(), i {
		left_product = (left_prefixes[i]) or 1
		right_product = (right_prefixes[nums.len() - i - 1]) or 1
		result.push(left_product * right_product)
	}

	return result
}

test_cases = map[[int...], [int...]] {
	[1,2,3,4]: [24,12,8,6],
	[-1,1,0,-3,3]: [0,0,9,0,0],
	[1, 2, 3, 4]: [24, 12, 8, 6],
	[2, 3, 4, 5]: [60, 40, 30, 24],
	[1, 1, 1, 1]: [1, 1, 1, 1],
	[10, 3, 5, 6, 2]: [180, 600, 360, 300, 900],
	[-1, 1, 0, -3, 3]: [0, 0, 9, 0, 0],
	[2, -2, 3, -3]: [18, -18, 12, -12],
	[-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
	[-5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]: [-1, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5, -5],
}.pairs()

print "🔵 Starting testing suite"

from 0 to test_cases.len(), i {
	[input, expected] = test_cases[i]

	output = product_of_array_except_self(input)
	
	if output != expected {
		print "🛑 Error in test suite: expected " + expected.to_str() + ", found: " + output.to_str()
		assert false
	}

	print "    ☐ Passed test #" + i
}

print "✅ Passed " + test_cases.len() + " test cases"