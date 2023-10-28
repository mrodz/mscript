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
	eval(r#"
		a = 7

		print_number = fn(input: int) {
		
			assert input == 10
	
		} # END print_number
	
		print_number(a += 3)
	
		assert a == 10
	"#).unwrap();
}

#[test]
#[should_panic = "cannot reassign using += to a, which is const"]
fn const_bin_op_mod() {
	eval(r#"
		const a = 7
		a += 1
		assert a == 8
	"#).unwrap();
}