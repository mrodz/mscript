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

#[test]
fn import_type() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import type Floof from alias
		import Dog from alias
		import alias

		cute: Floof = Dog("Scout")
		assert cute.name == "Scout"

		dog2: Floof = alias.Dog("Muna")
		assert dog2.to_string() == "Dog with name: Muna"
	"#,
    )
    .unwrap()
    .add(
        "alias.ms",
        r#"
		export class Dog {
			constructor(self, name: str) {
				self.name = name
			}
			name: str
			fn to_string(self) -> str {
				return "Dog with name: " + self.name
			}
		}

		export type Floof Dog
	"#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn forward_type_export() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import type Duck from duck.ms

		message: Duck = "Quack Quack"
	"#,
    )
    .unwrap()
    .add(
        "duck.ms",
        r#"
		import type Bugs from bugs

		export type Duck Bugs
	"#,
    )
    .unwrap()
    .add(
        "bugs.ms",
        r#"
		type Bunny str
		export type Bugs Bunny
	"#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
#[should_panic = "hint: `Bunny` is an alias for `str`, which isn't compatible with `int` in this context"]
fn forward_type_export_mismatch() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import type Duck from duck.ms

		message: Duck = 0xFEED
	"#,
    )
    .unwrap()
    .add(
        "duck.ms",
        r#"
		import type Bugs from bugs

		export type Duck Bugs
	"#,
    )
    .unwrap()
    .add(
        "bugs.ms",
        r#"
		type Bunny str
		export type Bugs Bunny
	"#,
    )
    .unwrap()
    .run()
    .unwrap()
}
