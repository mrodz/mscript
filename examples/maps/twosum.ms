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
