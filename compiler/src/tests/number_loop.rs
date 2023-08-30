use crate::eval;

#[test]
fn number_loop() {
	eval(r#"
		result = 0

		from 0 to 10 {
			result = result + 123
	  	}

		assert result == 1230
	"#).unwrap();
}

#[test]
fn number_loop_var() {
	eval(r#"
		result = 0

		from 0 to 10, n {
			result = result + 123 * n
	  	}

		assert result == 5535
	"#).unwrap();
}

#[test]
fn factorial() {
	eval(r#"
		factorial = fn(input: int) -> bigint {
			result: bigint = B1
	
			from 1 through input, n {
				result = result * n
			}
	
			return result
		}
	
		assert factorial(1) == 1
		assert factorial(2) == 2
		assert factorial(3) == 6
		assert factorial(4) == 24
		assert factorial(5) == 120
		assert factorial(6) == 720
		assert factorial(10) == 3628800
		assert factorial(30) == B0xD13F6370F96865DF5DD54000000
		assert factorial(33) == B8683317618811886495518194401280000000
	"#).unwrap();
}

#[test]
fn can_step() {
	eval(r#"
		result = 0
		
		from 1 to 100 step 2, n {
			result = result + n
		}

		assert result == 2500
	"#).unwrap();
}

#[test]
#[should_panic(expected = "explicitly defining a step property")]
fn bad_floats() {
	eval(r#"
		from 1.0 to 10, n { }
	"#).unwrap();
}

#[test]
fn good_floats() {
	eval(r#"
		from 1.0 to 10 step 1, n { }
	"#).unwrap();
}

#[test]
fn float_float_no_step() {
	eval(r#"
		from 3.14 to 22f, n { }
	"#).unwrap();
}

#[test]
fn bytes() {
	eval(r#"
		from 0b0 through 0b1111 step 0b1 { }
	"#).unwrap();
}

#[test]
fn proper_side_effect_of_loop() {
	// Remember: the variable will take the value of what the next element would be.
	eval(r#"
		number = 10

		from 0 through 3, number { }

		assert number == 4
	"#).unwrap();
}

#[test]
#[should_panic(expected = "use of undeclared variable")]
fn improper_use_of_loop_side_effect() {
	eval(r#"
		from 1 through 5, n {
			print n
		}
		print n
	"#).unwrap();
}

