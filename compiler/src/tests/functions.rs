use crate::eval;

#[test]
fn higher_order_callback() {
    eval(
        r#"
        apply = fn(f: fn(int) -> int, x: int) -> int {
            return f(x)
        }

        double = fn(x: int) -> int {
            return x * 2
        }

        triple = fn(x: int) -> int {
            return x * 3
        }

        assert apply(double, 5) == 10
        assert apply(triple, 5) == 15
        assert apply(fn(x: int) -> int { return x * x }, 4) == 16
    "#,
    )
    .unwrap()
}

#[test]
fn function_factory() {
    eval(
        r#"
        make_adder = fn(n: int) -> (fn(int) -> int) {
            return fn(x: int) -> int {
                return x + n
            }
        }

        add5 = make_adder(5)
        add10 = make_adder(10)

        assert add5(3) == 8
        assert add10(3) == 13
        assert add5(add10(1)) == 16
    "#,
    )
    .unwrap()
}

#[test]
fn immediately_invoked() {
    eval(
        r#"
        result = fn(x: int, y: int) -> int {
            return x + y
        }(10, 20)

        assert result == 30
    "#,
    )
    .unwrap()
}

#[test]
fn multi_capture_closure() {
    eval(
        r#"
        base = 100
        multiplier = 3
        offset = -10

        transform = fn(x: int) -> int {
            return base + x * multiplier + offset
        }

        assert transform(0) == 90
        assert transform(5) == 105

        multiplier = 2

        assert transform(5) == 100
    "#,
    )
    .unwrap()
}

#[test]
fn function_stored_in_array() {
    eval(
        r#"
        ops: [fn(int) -> int...] = [
            fn(x: int) -> int { return x + 1 },
            fn(x: int) -> int { return x * 2 },
            fn(x: int) -> int { return x * x },
        ]

        result = 3

        from 0 to ops.len(), i {
            result = (ops[i])(result)
        }

        # (3 + 1) = 4, (4 * 2) = 8, (8 * 8) = 64
        assert result == 64
    "#,
    )
    .unwrap()
}

#[test]
fn make_function() {
    eval(
        r#"
		add = fn(x: int, y: int) -> int {
			return x + y
		}

		assert add(1, 2) == 3
		assert add(5, -2) == 3
	"#,
    )
    .unwrap()
}

#[test]
fn capture_outside() {
    eval(
        r#"
		my_favorite_number = 17

		fmt_fav_number = fn() -> str {
			return "Your favorite number is " + my_favorite_number
		}

		assert fmt_fav_number() == "Your favorite number is 17"

		my_favorite_number = 2

		assert fmt_fav_number() == "Your favorite number is 2"
	"#,
    )
    .unwrap()
}

#[test]
fn false_modify() {
    eval(
        r#"
		my_favorite_number = 17

		change_favorite_number = fn() {
			my_favorite_number = 2
			assert my_favorite_number == 2
		}

		assert my_favorite_number == 17

		change_favorite_number()

		assert my_favorite_number == 17
	"#,
    )
    .unwrap()
}

#[test]
fn successful_modify() {
    eval(
        r#"
		my_favorite_number = 17

		change_favorite_number = fn() {
			modify my_favorite_number = 2
			assert my_favorite_number == 2
		}

		assert my_favorite_number == 17

		change_favorite_number()

		assert my_favorite_number == 2
	"#,
    )
    .unwrap()
}

#[test]
fn fibonacci() {
    eval(
        r#"
		fibonacci = fn(input: int) -> int {
			if input == 0 {
				return 0
			} else if input == 1 {
				return 1
			} else {
				return self(input - 1) + self(input - 2)
			}
		}
	
		assert fibonacci(15) == 610
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "type mismatch when calling function (argument #1 was expected to be `str` based on type signature, instead found `int`)"]
fn bad_self_arg() {
    eval(
        r#"
		x = fn(input: str) {
			while true {
				self(1)
			}
		}

		x("hello")
	"#,
    )
    .unwrap()
}
