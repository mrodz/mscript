length_of_longest_substring = fn(input: str) -> int {
    letter_frequency = map[str, bool]

    start_index = 0

    result = 0

    from 0 to input.len(), i {
        if letter_frequency[input[i]] != nil {
            length = i - start_index

            if length > result {
                result = length
            }

            letter_frequency.clear()
            letter_frequency[input[i]] = true

            start_index = i
            continue
        }
        
        letter_frequency[input[i]] = true
    }

    return result
}

print length_of_longest_substring("pwwkew")

