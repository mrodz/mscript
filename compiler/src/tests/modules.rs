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