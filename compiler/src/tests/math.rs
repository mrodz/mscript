use crate::eval;

#[test]
fn binary_operations() {
    eval(
        r#"
		a = 5
		b = 10
		assert a + b == 15
		assert 5 + 10 == 15

		assert 10 * 2 == 20
		assert 10 / 2 == 5
		assert 33 % 3 == 0

		assert true && true
		assert !(true && false)
		assert !(false && true)
		assert !(false && false)
		assert false || true
		assert true || false
		assert true ^ false
		assert false ^ true
		assert !(false ^ false)
		assert !(true ^ true)

		give_five = fn() -> int {
			return 5
		}

		assert give_five() > 4
		assert give_five() < 6
		assert give_five() >= 5
		assert give_five() <= 5
		assert give_five() >= 4
		assert give_five() <= 6

		assert true == true
		assert true != false

		assert "\n" * 3 == "\n\n\n"
		assert 5 * "\t" == "\t\t\t\t\t"
		assert 5 + "Hello" == "5Hello"
		assert "Hello" + 9.2 == "Hello9.2"

		assert 0b101 == give_five()
		assert 0b1111 == 15

		assert true

		assert 0b101 == 0x5 && 0x5 == B5 && B5 == 5f && 5F == 5 && B0x5 == 0b101
	"#,
    )
    .unwrap();
}

#[test]
fn lazy_eval() {
    eval(
        r#"
		truthy = fn() -> bool {
			return true
		}

		die = fn() -> bool {
			assert false
			return false
		}

		assert true || die()
		assert truthy() || die()

		traitor = false

		traitor && die()
	"#,
    )
    .unwrap();
}

#[test]
fn bin_op_assignment() {
    eval(
        r#"
		a = 7

		print_number = fn(input: int) {
		
			assert input == 10
	
		} # END print_number
	
		print_number(a += 3)
	
		assert a == 10
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic = "cannot reassign using += to a, which is const"]
fn const_bin_op_mod() {
    eval(
        r#"
		const a = 7
		a += 1
		assert a == 8
	"#,
    )
    .unwrap();
}

#[test]
fn string_bin_op_assignment() {
    eval(
        r#"
		result = "Fizz"

		result += "Buzz"

		assert result == "FizzBuzz"
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic = "invalid operation: int *= bool"]
fn type_mismatch_bin_op_assignment() {
    eval(
        r#"
		value = 10
		value *= false
	"#,
    )
    .unwrap();
}

#[test]
fn string_int_bin_op_assignment() {
    eval(
        r#"
		greeting = "Hello"
		greeting *= 3

		assert greeting == "HelloHelloHello"

		greeting += B777

		assert greeting == "HelloHelloHello777"

		greeting += "\n\t"

		assert greeting == "HelloHelloHello777\n\t"
	"#,
    )
    .unwrap();
}

#[test]
fn plus_equals() {
    eval(
        r#"
		a = 20
		b = a += 5

		assert a == 25
		assert b == 25
	"#,
    )
    .unwrap();
}

#[test]
fn minus_equals() {
    eval(
        r#"
		a = 20
		b = a -= 5

		assert a == 15
		assert b == 15
	"#,
    )
    .unwrap();
}

#[test]
fn times_equals() {
    eval(
        r#"
		a = 20
		b = a *= 5

		assert a == 100
		assert b == 100
	"#,
    )
    .unwrap();
}

#[test]
fn div_equals() {
    eval(
        r#"
		a = 20
		b = a /= 5

		assert a == 4
		assert b == 4
	"#,
    )
    .unwrap();
}

#[test]
fn mod_equals() {
    eval(
        r#"
		a = 20
		b = a %= 5

		assert a == 0
		assert b == 0
	"#,
    )
    .unwrap();
}

#[test]
fn class_member_bin_op_assignment() {
    eval(
        r#"

		class Person {
			first_name: str
			last_name: str
			age: int
	  
			constructor(self, first_name: str, last_name: str, age: int) {
				self.first_name = first_name
				self.last_name = last_name
				self.age = age
			}
	  
			fn grow_up(self) {
				self.age += 1
			}
	  
			fn get_full_name(self) -> str {
				return self.first_name + " " + self.last_name + " is " + self.age + " years old"
			}
		}
		
		mateo = Person("Mateo", "Rodriguez", 16)
		
		assert mateo.get_full_name() == "Mateo Rodriguez is 16 years old"
		
		mateo.grow_up()
		
		assert mateo.get_full_name() == "Mateo Rodriguez is 17 years old"

		
		class Wrapper {
	    	field: int
	    	constructor(self, field: int) {
    	    	self.field = field
	    	}
		}

		a = Wrapper(17)
		a.field += 3
		assert a.field == 20
	"#,
    )
    .unwrap();
}

#[test]
fn bit_ops() {
    eval(
        r#"
		assert 0b100 | 0b001 == 5
		assert 32 >> 2 == 8
		assert 32 << 2 == 128
		assert 0b10 & 0b01 == 0
		assert 0b11 & 0b10 == 0b10
		assert 0b01 xor 0b10 == 0b11
		assert 0b11 xor 0b11 == 0b00
	"#,
    )
    .unwrap()
}

#[test]
fn is_operator() {
    eval(
        r#"
		assert 5 is 5
		assert 0b101 is 5
		assert 0b101.abs() is (-5).abs()

		class A {}

		a1 = A()
		a2 = A()
		a3 = a1

		assert !(a1 is a2)
		assert a1 is a3

		const v1 = [1, 2, 3]
		const v2 = [1, 2, 3]
		const v3 = v2

		assert !(v1 is v2)
		assert v2 is v3
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic = "attempting to evaluate this expression at compile time resulted in an error"]
fn subtract_underflow() {
    eval(
        r#"
		a = 0b0111
		assert a.to_int().to_str() == "7"

		# 0b0 - 0b111 is an underflow
		from (0b0 - 0b111) to 5 step -1, i {
			print i
		}

		assert false
	"#,
    )
    .unwrap();
}

#[test]
fn array_index_bin_op_assignment() {
    eval(
        r#"
		
		positive_numbers: [int...] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		from 0 to positive_numbers.len(), i {
			positive_numbers[i] *= -1
		}

		assert positive_numbers == [-1, -2, -3, -4, -5, -6, -7, -8, -9, -10]
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic(expected = "invalid operation: int *= bool")]
fn unsupported_array_index_bin_op_assignment() {
    eval(
        r#"
		
		positive_numbers: [int...] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

		from 0 to positive_numbers.len(), i {
			positive_numbers[i] *= true
		}
	"#,
    )
    .unwrap();
}
