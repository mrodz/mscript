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
