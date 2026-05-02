use crate::eval;

#[test]
fn break_in_numeric_loop() {
    eval(
        r#"
        first_div_by_7 = -1
        from 1 through 100, n {
            if n % 7 == 0 {
                first_div_by_7 = n
                break
            }
        }
        assert first_div_by_7 == 7
    "#,
    )
    .unwrap();
}

#[test]
fn continue_in_numeric_loop() {
    eval(
        r#"
        sum_of_odds = 0
        from 1 through 10, n {
            if n % 2 == 0 {
                continue
            }
            sum_of_odds += n
        }
        # 1 + 3 + 5 + 7 + 9 = 25
        assert sum_of_odds == 25
    "#,
    )
    .unwrap();
}

#[test]
fn nested_loop() {
    eval(
        r#"
        pairs: [str...] = []
        from 0 to 3, i {
            from 0 to 3, j {
                if i != j {
                    pairs.push(i + "," + j)
                }
            }
        }
        # (0,1),(0,2),(1,0),(1,2),(2,0),(2,1) = 6 pairs
        assert pairs.len() == 6
        assert pairs[0] == "0,1"
        assert pairs[5] == "2,1"
    "#,
    )
    .unwrap();
}

#[test]
fn prime_check() {
    eval(
        r#"
        is_prime = fn(n: int) -> bool {
            if n < 2 {
                return false
            }

            i = 0
            from 2 to n, i {
                if n % i == 0 {
                    break
                }
            }

            return i == n
        }

        assert !is_prime(0)
        assert !is_prime(1)
        assert is_prime(2)
        assert is_prime(3)
        assert !is_prime(4)
        assert is_prime(5)
        assert !is_prime(9)
        assert is_prime(97)
    "#,
    )
    .unwrap();
}

#[test]
fn number_loop() {
    eval(
        r#"
		result = 0

		from 0 to 10 {
			result = result + 123
	  	}

		assert result == 1230
	"#,
    )
    .unwrap();
}

#[test]
fn number_loop_var() {
    eval(
        r#"
		result = 0

		from 0 to 10, n {
			result = result + 123 * n
	  	}

		assert result == 5535
	"#,
    )
    .unwrap();
}

#[test]
fn factorial() {
    eval(
        r#"
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
	"#,
    )
    .unwrap();
}

#[test]
fn can_step() {
    eval(
        r#"
		result = 0
		
		from 1 to 100 step 2, n {
			result = result + n
		}

		assert result == 2500
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic(expected = "explicitly defining a step property")]
fn bad_floats() {
    eval(
        r#"
		from 1.0 to 10, n { }
	"#,
    )
    .unwrap();
}

#[test]
fn good_floats() {
    eval(
        r#"
		from 1.0 to 10 step 1, n { }
	"#,
    )
    .unwrap();
}

#[test]
fn float_float_no_step() {
    eval(
        r#"
		from 3.14 to 22f, n { }
	"#,
    )
    .unwrap();
}

#[test]
fn bytes() {
    eval(
        r#"
		from 0b0 through 0b1111 step 0b1 { }
	"#,
    )
    .unwrap();
}

#[test]
fn proper_side_effect_of_loop() {
    // Remember: the variable will take the value of what the next element would be.
    eval(
        r#"
		number = 10

		from 0 through 3, number { }

		assert number == 4
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic(expected = "use of undeclared variable")]
fn improper_use_of_loop_side_effect() {
    eval(
        r#"
		from 1 through 5, n {
			print n
		}
		print n
	"#,
    )
    .unwrap();
}
