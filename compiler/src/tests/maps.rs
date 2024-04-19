use crate::eval;

#[test]
fn basic_map() {
    eval(
        r#"
		ages = map[str, int] {
			"Mateo": 17,
			"Dylan": 16,
			"Scout": 7,
			"Daniel": 55,
		}

		empty_map = map[int, int]
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "This map expects keys with type `int`, but instead found type `str`"]
fn key_mismatch() {
    eval(
        r#"
		is_even = map[int, bool] {
			1: false,
			2: true,
			3: false,
			"Dog": true,
		}
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "This map expects values with type `bool`, but instead found type `map[int, int]`"]
fn value_mismatch() {
    eval(
        r#"
		is_even = map[int, bool] {
			1: false,
			2: true,
			3: map[int, int],
			4: true,
		}
	"#,
    )
    .unwrap()
}

#[test]
fn map_properties() {
    eval(
        r#"
		ages = map[str, int] {
			"Mateo": 17,
			"Dylan": 16,
			"Scout": 7,
			"Daniel": 55,
		}
		
		assert ages.len() == 4
		
		ages["Mateoo"] = 200

		assert ages.len() == 5

		ages["Mateo"] = 16

		assert ages.len() == 5
		assert ages["Mateo"] == 16		
		
		assert ages.contains_key("Mateo")
		assert !ages.contains_key("Davey")

		assert ages.replace("Davey", 16) == nil
		assert ages["Davey"] == 16
		
		assert ages["DDDD"] == nil
		assert !ages.contains_key("DDDD")
		
		assert ages.replace("Mateo", 18) == 16
		assert ages["Mateo"] == 18
		
		pairs = ages.pairs()
		
		special_sum = 0

		from 0 to pairs.len(), i {
			[key, value] = pairs[i]
			special_sum += key.len() * value
		}

		assert special_sum == 1815

		key_lengths_squared = ages.keys().map(fn(key: str) -> int {
			return key.len().pow(2).to_int()
		})

		key_length_sum = 0

		from 0 to key_lengths_squared.len(), i {
			key_length_sum += key_lengths_squared[i]
		}

		assert key_length_sum == 172

		values_pow = ages.values().map(fn(value: int) -> bigint {
			return value.pow(3)
		})

		value_pow_sum: bigint = B0

		from 0 to values_pow.len(), i {
			value_pow_sum += values_pow[i]
		}

		assert value_pow_sum == 8180742
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "`map[int, int]` does not support indexing for `str` (Hint: this type does support indexes of types int)"]
fn map_improper_index() {
    eval(
        r#"
		my_map = map[int, int] {
			1: 1,
			2: 4,
			3: 9
		}

		my_map["hi"] = 3
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "type mismatch: cannot assign `str` to `int`"]
fn map_improper_value() {
    eval(
        r#"
		my_map = map[int, int] {
			1: 1,
			2: 4,
			3: 9
		}

		my_map[4] = "world"
	"#,
    )
    .unwrap()
}

#[test]
fn two_sum() {
    eval(
        r#"
	
		###
		This is the O(n^2) implementation.
		@see https://leetcode.com/problems/two-sum/
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

		assert two_sum([2,7,11,15], 9) == [0,1]
		assert two_sum([3,2,4], 6) == [1,2]
		assert two_sum([3,3], 6) == [0,1]
		assert two_sum([1, 2, 3, 4, 5, 6], 100) == nil

	"#,
    )
    .unwrap()
}
