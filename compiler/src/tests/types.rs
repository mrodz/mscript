use crate::{eval, EvalEnvironment};

#[test]
fn type_alias() {
    eval(
        r#"
		type Miles float

		const marathon: Miles = 26f

		type Kilometer float
		miles_to_km = fn(input: Miles) -> Kilometer {
			return input * 1.609
		}

		assert miles_to_km(4f) == 6.436
	"#,
    )
    .unwrap();
}

#[test]
fn many_types() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import alias

		assert alias.x() == 20000
	"#,
    )
    .unwrap()
    .add(
        "alias.ms",
        r#"
		type Miles int

		marathon: Miles = 6
	
		assert marathon - 1 == 5
	
		export x: fn() -> int = fn() -> int {
			type Meters int
	
			a: Meters = 10
			b: Miles = 20
			c: int = a
			d: Miles = a
	
			return a * b * c * d
		}
	"#,
    )
    .unwrap()
    .run()
    .unwrap()
}
