use_state = fn(initial_value: int) -> [fn() -> int, fn(int)] {
	data: int = initial_value

	temp = fn(update: int) {
		modify data = update
	}

	return [
		fn() -> int {
			return data
		},
		temp
	]
} 

packed = use_state(99)
get_data = packed[0]
set_data = packed[1]

print get_data
print set_data

print get_data()
set_data(10)
print get_data()

use_counter = fn(init: int) -> [fn() -> int, fn(), fn()] {
	state = init
	return [
		fn() -> int {
			return state
		},
		fn() {
			modify state = state + 1
		},
		fn() {
			modify state = state - 1
		}
	]
}

print "\n" * 5

counter_items = use_counter(20)
count = counter_items[0]
inc = counter_items[1]
dec = counter_items[2]

print count()
inc()
print count()
dec()
dec()
print count()