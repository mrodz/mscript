use crate::eval;

#[test]
fn if_statement() {
    eval(r#"
        a = 10

        if true {
            # ...
        } else {
            assert false
        }

        if false {
            assert false
        }

        assert a == 10
    "#).unwrap();
}

#[test]
fn if_statement_in_function() {
    eval(r#"
        b = 50
        const C = 33
        do_something = fn() {
            a = 10

            if true {
                # ...
            } else {
                assert false
            }

            if false {
                assert false
            }

            assert a == 10
            assert a + b * C == 1660
        }

        do_something()

    "#).unwrap();
}

#[test]
fn nesting() {
    eval(r#"
        if true {
            if false {
                assert false
            } else {
                # ...
            }
        } else {
            assert false
        }

        if false {
            assert false
        } else if true {
            # ...
        } else {
            assert false
        }
    "#).unwrap();
}

#[test]
fn if_else_inside_numeric_loop() {
    eval(r#"
        fizz_buzz = fn(input: int) -> str? {
            result: str? = nil
    
            if input % 3 == 0 {
                result = "Fizz"
            }
    
            if input % 5 == 0 {
                preappend = get result or ""
                result = preappend + "Buzz"
            }
    
            return result	
        }
    
        result_buffer = ""

        from 1 through 15, i {
            if label ?= fizz_buzz(i) {
                result_buffer = result_buffer + label
            } else {
                result_buffer = result_buffer + i
            }
        }

        assert result_buffer == "12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz"
    "#).unwrap();
}

#[test]
fn if_else_inside_while_loop() {
    eval(r#"
        fizz_buzz = fn(input: int) -> str? {
            result: str? = nil
    
            if input % 3 == 0 {
                result = "Fizz"
            }
    
            if input % 5 == 0 {
                preappend = get result or ""
                result = preappend + "Buzz"
            }
    
            return result	
        }
    
        result_buffer = ""

        i = 1

        while i <= 15 {
            if label ?= fizz_buzz(i) {
                result_buffer = result_buffer + label
            } else {
                result_buffer = result_buffer + i
            }

            i = i + 1
        }

        assert result_buffer == "12Fizz4BuzzFizz78FizzBuzz11Fizz1314FizzBuzz"
    "#).unwrap();
}