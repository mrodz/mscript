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
