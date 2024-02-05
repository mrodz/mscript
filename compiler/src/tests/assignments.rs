use crate::{eval, EvalEnvironment};

/// To update a variable, just write:
///
/// a = 20
///
/// The "modify" keyword will try to update a variable capture, which
/// does not exist at the module level.
#[test]
#[should_panic = "this assignment contains the \"modify\" attribute, which is used to mutate a variable from a higher scope"]
fn modify_misuse() {
    eval(
        r#"
		a = 10
		modify a = 20
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "attempting to reassign to a const variable"]
fn attempt_const_bypass_1() {
    eval(
        r#"
		const a = 5

		if true {
			a = 7
		}
		
		assert typeof a == "int"
		assert a == 5 

		assert false
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "attempting to reassign to a const variable"]
fn attempt_const_bypass_2() {
    eval(
        r#"
		const a = 5

		fn() {
			modify a = 7
		}()
		
		assert typeof a == "int"
		assert a == 5 

		assert false
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "attempting to reassign to a const variable"]
fn attempt_const_bypass_3() {
    eval(
        r#"
		const a = 5

		fn() {
			modify a = 7
		}()
		
		assert typeof a == "int"
		assert a == 5 

		assert false
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "attempting to reassign to a const variable"]
fn attempt_const_bypass_4() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import book
		book = 5
	"#,
    )
    .unwrap()
    .add("book.ms", "")
    .unwrap()
    .run()
    .unwrap();
}

#[test]
#[should_panic = "The target of this assignment is const, and cannot be modified"]
fn attempt_const_bypass_5() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import book
		book.favorite = "Harry Potter"
	"#,
    )
    .unwrap()
    .add(
        "book.ms",
        r#"
		export const favorite: str = "The Kite Runner"
	"#,
    )
    .unwrap()
    .run()
    .unwrap();
}

#[test]
fn not_import_const_bypass() {
    EvalEnvironment::entrypoint(
        "main.ms",
        r#"
		import favorite from book
		import book

		favorite = "Harry Potter"

		assert book.favorite == "The Kite Runner"

		assert favorite == "Harry Potter"
	"#,
    )
    .unwrap()
    .add(
        "book.ms",
        r#"
		export const favorite: str = "The Kite Runner"
	"#,
    )
    .unwrap()
    .run()
    .unwrap();
}

#[test]
#[should_panic = "type mismatch: this assignment will update a variable with type `str`, which is not compatible with the original type `int`"]
fn attempt_type_bypass_1() {
    eval(
        r#"
		a = 5

		while true {
			a = "hello"
			break
		}
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "type mismatch: this assignment will update a variable with type `str`, which is not compatible with the original type `int`"]
fn attempt_type_bypass_2() {
    eval(
        r#"
		a = 5

		fn() {
			a = [1, 2, 3].map(fn (x: int) -> int {
					return x
				})
			# ^ okay because a is exclusive to this scope,
			#   and not a captured reference.
		}

		fn() {
			modify a = "hello"
		}()
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "this unpack operation shadows one or more names accesible from this scope, starting at `set_text`"]
fn attempt_type_bypass_3() {
    eval(
        r#"
		const use_text = fn(initial: str) -> [fn() -> str, fn(str)] {
			return [
				fn() -> str {
					return initial
				},
				fn(new_text: str) {
					modify initial = new_text
				} 
			]
		}

		set_text = fn() {
			print "hello world!"
		}

		print typeof set_text

		if true {
			[get_text, set_text] = use_text("Guest")
		}

		assert get_text() == "Guest"
		set_text() # which `set_text` is valid?
		assert get_text() == "@mrod"
		print typeof set_text
	"#,
    )
    .unwrap()
}

#[test]
#[should_panic = "this name shadows a variable with type `str` in a higher scope, but this iteration construct yields `int` and is not compatible"]
fn attempt_type_bypass_4() {
    eval(
        r#"
		a = "hello"

		from 1 to 10, a {
			print a
		}

		print a
		print typeof a
	"#,
    )
    .unwrap()
}
