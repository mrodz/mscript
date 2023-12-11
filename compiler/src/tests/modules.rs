use crate::EvalEnvironment;

#[test]
fn testing_framework() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		assert 1 + 2 == 3
	"#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn two_files() {
    EvalEnvironment::entrypoint(
        "a.ms",
        r#"
		import b

		print b

		assert b.MESSAGE == "Hello World!"

		print b
		print b.MESSAGE

	"#,
    )
    .unwrap()
    .add(
        "b.ms",
        r#"
		export const MESSAGE: str = "Hello World!"
		"#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn many_files() {
    EvalEnvironment::entrypoint(
        "a.ms",
        r#"
		import b
		import c.ms
		import d

		assert b.value + c.ValueGiver().give() + d.my_fav_number() == 94
	"#,
    )
    .unwrap()
    .add(
        "b.ms",
        r#"
			export value: int = 5
		"#,
    )
    .unwrap()
    .add(
        "c.ms",
        r#"

			export class ValueGiver {
				fn give(self) -> int {
					return 69
				}
			}

		"#,
    )
    .unwrap()
    .add(
        "d.ms",
        r#"
			
			export my_fav_number: fn() -> int = fn() -> int {
				return 20
			}

		"#,
    )
    .unwrap()
    .run()
    .unwrap()
}
