use crate::eval;

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
