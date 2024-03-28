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

#[test]
fn different_call_forms() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		
        import lib

        assert lib.version == "v1.3.25"

        class Wrapper {
            fn drawPixel2(self, x: int, y: int) -> str {
                return "WRAPPER! " + lib.drawPixel(x, y)
            }
        }

        w = Wrapper()
        assert w.drawPixel2(50, 60) == "WRAPPER! Drawing pixel at: (50, 60)"
        assert (Wrapper()).drawPixel2(-5, 6) == "WRAPPER! Drawing pixel at: (-5, 6)"

        shadow = lib.drawPixel

        assert shadow(45, -69) == "Drawing pixel at: (45, -69)"

        assert lib.drawPixel(0, 3) == "Drawing pixel at: (0, 3)"
	"#,
    )
    .unwrap()
    .add(
        "lib.ms",
        r#"
            print "Loading Graphics Library..."

            export const version: str = "v1.3.25"

            export const drawPixel: fn(int, int) -> str = fn(x: int, y: int) -> str {
                return "Drawing pixel at: (" + x + ", " + y + ")"
            }
		"#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn import_name() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        import Dog from doggies
        import Cat from kitties.ms

        pup1 = Dog("Scout")
        cat1 = Cat(999)

        import doggies
        import kitties

        pup2 = doggies.Dog("Scout")
        cat2 = kitties.Cat(999)

        assert cat1.cuteness == cat2.cuteness
        assert pup1.name == pup2.name

        assert Cat is kitties.Cat
        assert Dog is doggies.Dog
    "#,
    )
    .unwrap()
    .add(
        "doggies.ms",
        r#"
        export class Dog {
            constructor(self, name: str) {
                self.name = name
            }
            name: str
            fn eq(self, other: Self) -> bool {
                return self.name == other.name
            }
        }
    "#,
    )
    .unwrap()
    .add(
        "kitties.ms",
        r#"
        export class Cat {
            cuteness: int
            constructor(self, cuteness: int) {
                self.cuteness = cuteness
            }
            fn eq(self, other: Self) -> bool {
                return self.cuteness == other.cuteness
            }
        }
    "#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn circular_import_workaround() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        import CognitoUser, type CognitoFlags, VERSION from aws_cognito
        import shared

        user = CognitoUser("Mateo", "mateo@email.com")

        print VERSION

        x: CognitoFlags = 0b101
        shared.take_user(x)
    "#,
    )
    .unwrap()
    .add(
        "aws_cognito.ms",
        r#"
        print "sjjjj"
        export const VERSION: str = "v0.0.1"

        export type CognitoFlags byte

        print "aws_cognito library, " + VERSION

        export class CognitoUser {
	        name: str
	        email: str

	        constructor(self, name: str, email: str) {
    	    	self.name = name
	    	    self.email = email
	        }
        }

        import take_user from shared

        take_user(0b101)
    "#,
    )
    .unwrap()
    .add(
        "shared.ms",
        r#"
        import type CognitoFlags from aws_cognito

        export take_user: fn(CognitoFlags) = fn(input: CognitoFlags) {
	        print input
        }
    "#,
    )
    .unwrap()
    .run()
    .unwrap();
}

#[test]
fn import_self() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        export hello: int = 5

        import main
        print main
        print typeof main
        # assert main.hello == hello
    "#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn forward_export() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        import number from numbers.ms
        import numberville

        assert number == numberville.secret
        assert number == 5
        assert numberville.secret == 5
    "#,
    )
    .unwrap()
    .add(
        "numbers.ms",
        r#"
        import secret from numberville
        export number: int = secret
    "#,
    )
    .unwrap()
    .add(
        "numberville.ms",
        r#"
        export const secret: int = 5
    "#,
    )
    .unwrap()
    .run()
    .unwrap()
}

#[test]
fn dependent_hidden_type_exports() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        # these functions cannot be called, because their inputs are hidden and cannot be constructed in main.ms
        import take_kilometer, take_mile from other
    "#,
    )
    .unwrap()
    .add("other.ms", r#"
        type Kilometer int
        class Mile {}

        export take_kilometer: fn(Kilometer) = fn(input: Kilometer) {
            print input
        }        

        export take_mile: fn(Mile) = fn(input: Mile) {
            print input
        }
    "#)
    .unwrap()
    .run()
    .unwrap()
}

#[test]
#[should_panic = "`other.ms` has no visible member `Mile`"]
fn dependent_hidden_type_exports_errors_ok() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
        import take_kilometer from other

        take_kilometer(5)

        # this function cannot be called, because its inputs are hidden and cannot be constructed in main.ms
        import take_mile, Mile from other
        take_mile(Mile())
    "#,
    )
    .unwrap()
    .add("other.ms", r#"
        type Kilometer int
        class Mile {}

        export take_kilometer: fn(Kilometer) = fn(input: Kilometer) {
            assert input == 5
        }        

        export take_mile: fn(Mile) = fn(input: Mile) {
            # should never run
            assert false
        }
    "#)
    .unwrap()
    .run()
    .unwrap()
}
