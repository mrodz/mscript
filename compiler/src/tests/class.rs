use crate::eval;

#[test]
fn class_grammar() {
    eval(
        r#"
		class Person {
			first_name: str
			last_name: str
			age: int
	
			constructor(self, first_name: str, last_name: str, age: int) {
				self.first_name = first_name
				self.last_name = last_name
				self.age = 16
			}
	
			fn grow_up(self) {
				self.age = self.age + 1
			}	
	
			fn full_name(self) -> str {
				return self.first_name + " " + self.last_name
			}
		}
	"#,
    )
    .unwrap()
}

#[test]
fn complex_number() {
    eval(
        r#"
		class ImaginaryNumber {
			real: float
			imaginary: float

			constructor(self, real: float, imaginary: float) {
				self.real = real
				self.imaginary = imaginary
			}

			fn to_string(self) -> str {
				return "" + self.real + " + " + self.imaginary + "i"
			}
		}

		sqrtNeg1 = ImaginaryNumber(0f, 1f)
		assert sqrtNeg1.to_string() == "0 + 1i"
	"#,
    )
    .unwrap();
}

#[test]
fn instance_counter() {
    eval(
        r#"
		DOGGOS = 0

		class Dog {
			id: int

			constructor(self) {
				self.id = DOGGOS
				modify DOGGOS = DOGGOS + 1
			}

			fn to_string(self) -> str {
				return "Doggo #" + self.id
			}
		}

		scout = Dog()
		muna = Dog()
		checkers = Dog()

		assert DOGGOS == 3

		assert scout.to_string() == "Doggo #0"
		assert muna.to_string() == "Doggo #1"
		assert checkers.to_string() == "Doggo #2"
		
	"#,
    )
    .unwrap();
}

#[test]
fn factorial_pair() {
    eval(
        r#"
		class FactorialPair {
			number: int
			factorial: bigint

			constructor(self, number: int) {
				self.number = number
				self.set_factorial()
			}

			fn set_factorial(self) {
				result: bigint = B1

				from 0x1 through self.number, n {
					result = result * n
				}

				self.factorial = result
			}

			fn to_string(self) -> str {
				return "Factorial of " + self.number + " is " + self.factorial
			}
		}

		fact6 = FactorialPair(6)
		fact1 = FactorialPair(1)
		fact32 = FactorialPair(32)

		assert fact1.to_string() == "Factorial of 1 is 1"
		assert fact6.to_string() == "Factorial of 6 is 720"
		assert fact32.to_string() == "Factorial of 32 is 263130836933693530167218012160000000"
	"#,
    )
    .unwrap();
}

#[test]
fn capture_outside_env() {
    eval(
        r#"
		MESSAGE = "Hello, "
		GREETER_INSTANCES = 0

		class Greeter {
			name: str

			constructor(self, name: str) {
				self.name = name
				modify GREETER_INSTANCES = GREETER_INSTANCES + 1
			}

			fn greet(self) -> str {
				return MESSAGE + self.name
			}

			fn number_of_greeters(self) -> int {
				return GREETER_INSTANCES
			}
		}

		hi_mateo = Greeter("Mateo")
		hi_scout = Greeter("Scout")

		assert hi_mateo.greet() == "Hello, Mateo"
		assert hi_scout.greet() == "Hello, Scout"

		assert hi_mateo.number_of_greeters() == hi_scout.number_of_greeters()
		assert hi_mateo.number_of_greeters() == GREETER_INSTANCES
		assert GREETER_INSTANCES == 2
	"#,
    )
    .unwrap();
}

#[test]
fn self_type() {
	eval(r#"
		class Dog {
			name: str
		
			constructor(self, name: str) {
				self.name = name
			}
	
			fn play(self, other: Self) -> str {
				return self.name + " is playing with " + other.name
			}
		}
	
		x = Dog("Scout")
		y = Dog("Muna")
	
		assert x.play(y) == "Scout is playing with Muna"
	"#).unwrap();
}

#[test]
fn self_type_1() {
	eval(r#"
		class A {
			value: int
			constructor(self) {
				self.value = 5
			}
			fn make_new_a(self) -> Self {
				new: Self = Self()
				new.value = 10
				return new
			}
			fn to_string(self) -> str {
				return "A("+self.value+")"
			}
		}
	
		a = A()
		b = a.make_new_a()
	
		assert a.to_string() == "A(5)"
		assert b.to_string() == "A(10)"
	"#).unwrap();
}

#[test]
fn self_type_2() {
	eval(r#"
		class Dog {
			name: str
		
			constructor(self, name: str) {
				self.name = name
			}
	
			fn play(self, other: Self) -> str {
				return self.name + " is playing with " + other.name
			}
	
			fn birds_and_bees(self, other: Self) -> Self {
				baby_name = self.name + " & " + other.name + "'s Baby"
				return Self(baby_name)
			}
		}
	
		x = Dog("Scout")
		y = Dog("Muna")
		
		assert x.play(y) == "Scout is playing with Muna"
	
		pup = x.birds_and_bees(y)
	
		assert pup.name == "Scout & Muna's Baby"
	"#).unwrap();
}

#[test]
fn chain() {
	eval(r#"
		class D {
			fn get_value(self) -> int {
				return 42
			}
		}
	
		class C {
			fn d(self) -> D {
				return D()
			}
		}
	
		class B {
			fn c(self) -> C {
				return C()
			}
		}
	
		class A {
			fn b(self) -> B {
				return B()
			}
		}
	
		assert (A()).b().c().d().get_value() == 42
	"#).unwrap();
}