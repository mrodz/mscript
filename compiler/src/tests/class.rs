use crate::eval;

#[test]
fn class_grammar() {
    eval(r#"
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
	eval(r#"
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
	"#).unwrap();
}