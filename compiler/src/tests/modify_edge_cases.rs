use crate::eval;

#[test]
fn crud() {
    eval(
        r#"
		a = 5

		increment_a = fn() {
			modify a = a + 5
		}
	
		assert a == 5
		increment_a()
		assert a == 10
		increment_a()
		assert a == 15
	
		test2 = fn() -> int {
			x = 10
			y = 30
			b = 50
			return x + y + b + a
		}
	
		assert test2() == 105
	"#,
    )
    .unwrap();
}

#[test]
fn closures() {
    eval(
        r#"
		x = 5

		adder = fn(b: int) -> (fn(int) -> int) {
			return fn(input: int) -> int {
				return input + b
			}
		}

		add_twenty = adder(20)
		assert add_twenty(5) == 25
	"#,
    )
    .unwrap();
}
