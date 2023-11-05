use crate::EvalEnvironment;

#[test]
fn testing_framework() {
	EvalEnvironment::entrypoint("main.ms", r#"
		assert 1 + 2 == 3
	"#)
		.unwrap()
		.run()
		.unwrap()
}

#[test]
fn two_files() {
	EvalEnvironment::entrypoint("a.ms", r#"
		import b

		assert b.MESSAGE == "Hello World!"

		print b
		print b.MESSAGE

	"#)
		.unwrap()
		.add("b.ms", r#"
			const MESSAGE = "Hello World!"

			export { MESSAGE }
		"#)
		.unwrap()
		.run()
		.unwrap()
}

#[test]
fn many_files() {
	EvalEnvironment::entrypoint("a.ms", r#"
		import b
		import c.ms
		import d

		assert b.value + c.ValueGiver().give() + d.my_fav_number() == 94
	"#)
		.unwrap()
		.add("b.ms", r#"
			value = 5

			export { value }
		"#)
		.unwrap()
		.add("c.ms", r#"

			class ValueGiver {
				fn give(self) -> int {
					return 69
				}
			}

			export { ValueGiver }

		"#)
		.unwrap()
		.add("d.ms", r#"
			
			my_fav_number = fn() -> int {
				return 20
			}

			export { my_fav_number }

		"#)
		.unwrap()
		.run()
		.unwrap()
}