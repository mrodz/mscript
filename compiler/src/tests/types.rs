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
#[ignore = "2/4/2024 - type updates are no longer in spec"]
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
#[ignore = "2/4/2024 - type updates are no longer in spec"]
fn type_updates_index() {
    eval(
        r#"
        x = "hi"
        x: [int...] = [1, 2, 3]
    
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

#[test]
fn list_with_optionals() {
    eval(
        r#"
        type numberz int

        xxx: numberz = 99

        type unicorn numberz

        yyy: unicorn? = 100

        assert typeof yyy == "unicorn?"

        a: [int?...] = [5, xxx, nil, -1]

        result: [str...] = []

        from 0 to a.len(), i {
            result.push((a[i]).to_str() + ": " + typeof a[i])
        }

        assert result == ["5: int?", "99: int?", "nil: int?", "-1: int?"]
    "#,
    )
    .unwrap()
}

/// The expected message provided is very deep in the error hint, so if it is encountered,
/// it is likely to be correct.
///
/// ## Expected Hint
/// <pre>
/// $__EVAL_ENV__.ms:12:12
///    |
/// 12 |         a: [int?...] = [5, xxx, nil, yyy]
///    |            ^--------^
///    |
///    = declaration wanted `[int?...]`, but value is `[int, numberz, nil, unicorn?]`
///         + hint: value at index 3 has type `unicorn?`, which is not compatible with `int?`
///             + hint: these optionals have differing underlying types: `unicorn` is not compatible with `int`
///                 + hint: `unicorn` is an alias for `numberz?`
///                     + hint: unwrap this optional to use its value using the `get` keyword, or provide a fallback with the `or` keyword
///                         + hint: `numberz` is an alias for `int` << HERE IS WHAT WE'RE TESTING FOR
/// </pre>
#[test]
fn list_with_optionals_type_mismatch() {
    eval(
        r#"
        type numberz int

        xxx: numberz = 99

        type unicorn numberz

        yyy: numberz = 100

        assert typeof yyy == "numberz"

        a: [unicorn?...] = [5, xxx, nil, yyy]

        result = ""

        from 0 to a.len(), i {
            result += (a[i]).to_str() + ": " + typeof a[i]
        }

        assert result == "5: unicorn?99: unicorn?nil: unicorn?100: unicorn?"
    "#,
    )
    .unwrap()
}

#[test]
fn list_with_optional_class() {
    eval(
        r#"
        class Dog {
            name: str
            constructor(self, name: str) {
                self.name = name
            }
            fn to_str(self) -> str {
                return "Dog named " + self.name
            }
        }
        
        class Cat {
            name: str
            breed: str
            constructor(self, name: str, breed: str) {
                self.name = name
                self.breed = breed
            }
            fn to_str(self) -> str {
                return "Cat named " + self.name + " is a " + self.breed
            }
        }
        
        dogs: [Dog...] = [Dog("Old Yeller"), Dog("Air Bud"), Dog("Odie")]
        
        dogs.push(Dog("Scout"))
        dogs.push(Dog("Muna"))
        
        assert dogs.len() == 5
        assert (dogs[3]).name == "Scout"
        
        dogs_str: [str...] = []
        from 0 to dogs.len(), i {
            dogs_str.push((dogs[i]).to_str() + ": " + typeof dogs[i])
        }
        
        assert dogs_str == ["Dog named Old Yeller: Dog", "Dog named Air Bud: Dog", "Dog named Odie: Dog", "Dog named Scout: Dog", "Dog named Muna: Dog"]
        
        dogs.reverse()
        
        maybe_dogs = dogs.map(fn(x: Dog) -> Dog? { return x })
        
        assert typeof maybe_dogs == "[Dog?...]"
        assert dogs == maybe_dogs
        
        maybe_dogs_str: [str...] = []
        
        from 0 to maybe_dogs.len(), i {
            maybe_dogs_str.push((maybe_dogs[i]).to_str() + ": " + typeof maybe_dogs[i])
        }
        
        assert maybe_dogs_str == ["Dog named Muna: Dog?", "Dog named Scout: Dog?", "Dog named Odie: Dog?", "Dog named Air Bud: Dog?", "Dog named Old Yeller: Dog?"]
        
        cats = maybe_dogs.map(fn(x: Dog?) -> Cat { return Cat((get x).name, "tabby") })
        
        cats_str: [str...] = []
        from 0 to cats.len(), i {
            cats_str.push((cats[i]).to_str() + ": " + typeof cats[i])
        }
        
        assert cats_str == ["Cat named Muna is a tabby: Cat", "Cat named Scout is a tabby: Cat", "Cat named Odie is a tabby: Cat", "Cat named Air Bud is a tabby: Cat", "Cat named Old Yeller is a tabby: Cat"]
    "#,
    )
    .unwrap();
}
