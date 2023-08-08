[a, b, c] = [1, 2, 3]

print a
print b
print c

print "\nReact-like design for hooks xD\n"

use_state = fn(state: int) -> [fn() -> int, fn(int)] {
	return [
		fn() -> int {
			return state
		},
		fn(new_state: int) {
			modify state = new_state
		}
	]
}

[get_state, set_state] = use_state(50)
print get_state()
set_state(10)
print get_state()