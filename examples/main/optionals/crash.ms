fizz_buzz = fn(input: int) -> str? {
            result: str? = nil
    
            if input % 3 == 0 {
                result = "Fizz"
            }
    
            if input % 5 == 0 {
                preappend = get result or ""
                result = preappend + "Buzz"
            }
    
            return result	
        }
    
        result_buffer = ""

        i = 1

        while i <= 15 {
            label: str? = nil
            if label ?= fizz_buzz(i) {
                result_buffer = result_buffer + label
            } else {
                result_buffer = result_buffer + i
            }

            i = i + 1
        }

        assert result_buffer == "12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz"