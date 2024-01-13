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
#[should_panic = "hint: `Bunny` is an alias for `str`"]
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

#[test]
fn types_with_reassignment() {
    EvalEnvironment::entrypoint(
        "a.ms",
        r#"
        import type Foo, type Bar, Fizz from b

        wooza: Foo = Fizz(42)
        const num: Bar = "hello"

        assert (wooza.do_math(8) - 4) * 3 == 63 

        accept_string_like = fn(input: str) -> str {
            return input + "!"
        }

        assert accept_string_like(num) == "hello!"
        assert accept_string_like("basic") == "basic!"
    "#,
    )
    .unwrap()
    .add(
        "b.ms",
        r#"
        type NumberzAreKool int

        export class Fizz {
            state: int
            constructor(self, init: NumberzAreKool) {
                self.state = init
            }
            fn do_math(self, other: int) -> NumberzAreKool {
                return (self.state + other) / 2
            }
        }

        export type Foo Fizz

        type Stool str
        export type Bar Stool
    "#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn type_updates() {
    eval(
        r#"
        x = "hello"
        x = 42

        take_x = fn(input: int) {
	        assert input == 42
        }

        take_x(x)
        "#,
    )
    .unwrap()
}

/// `2` is an invalid index into "hi", so this test asserts that types and values are being updated
#[test]
fn type_updates_index() {
    eval(
        r#"
        x = "hi"
        x = [1, 2, 3]
    
        assert x[2] == 3
    "#,
    )
    .unwrap()
}

#[test]
#[should_panic = "cannot index with `3` into list whose known valid safe indexes are 0..2"]
fn invalid_index() {
    eval(
        r#"
        x = "hi"
        x = [1, 2, 3]
    
        x[3]
    "#,
    )
    .unwrap()
}

/// No assertions, just type checking for this test
#[test]
fn class_self_aliases() {
    eval(
        r#"
        INSTANCE_COUNTER = 0

        class A {
            constructor(self) {
                INSTANCE_COUNTER += 1
                
                ###
                this object should never be made in any of the `or` 
                statement fallbacks, because they aren't nil
                ###

                assert INSTANCE_COUNTER <= 3
            }
    
            fn assoc(self) -> Self? {
                return self
            }
        }
    
        a = A()
        
        type B A
        type C B
    
        take_a = fn(in: C) {
            print in
        }

        b: B = A()
    
        print a
        print a.assoc()
        take_a(a)
        take_a(b or A())
        take_a((a.assoc()) or A())
        take_a((A()) or get a.assoc())
    "#,
    )
    .unwrap()
}

#[test]
#[should_panic = "type mismatch when calling function (argument #1 was expected to be `C` based on type signature, instead found `A?`)"]
fn class_self_aliases_forget_unwrap() {
    eval(
        r#"
        class A {
            fn assoc(self) -> Self? {
                return self
            }
        }
    
        a = A()
        
        type B A
        type C B
    
        take_a = fn(in: C) {
            print in
        }

        b: B = A()
    
        print a
        print a.assoc()
        take_a(a)
        take_a(b)
        take_a(a.assoc())
    "#,
    )
    .unwrap()
}
