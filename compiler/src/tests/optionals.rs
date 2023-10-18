use crate::eval;

#[test]
#[should_panic = "unwrap this optional to use its value"]
fn type_mismatch() {
    eval(
        r#"
        give_name = fn(input: int) -> str? {
            if input == 42 {
                return "Mateo"
            } else {
                return nil
            }
        }
    
        maybe_me: str = give_name(42)
    "#,
    )
    .unwrap();
}

#[test]
fn optional_return_type() {
    eval(
        r#"
        give_name = fn(input: int) -> str? {
            if input == 42 {
                return "Mateo"
            } else {
                return nil
            }
        }
    
        x: str? = give_name(30)
    
        assert give_name(30) == nil
        assert give_name(42) == "Mateo"
        assert give_name(1) == nil
        assert x == nil
    "#,
    )
    .unwrap();
}

#[test]
fn normal_assignment_with_try_equals() {
    eval(
        r#"
            a = 5
            b ?= 10
            assert a + b == 15
        "#,
    )
    .unwrap();
}

#[test]
fn assignment_as_expr() {
    eval(
        r#"
            c: int? = nil

            a = b ?= c

            assert b == nil
            assert !a
            assert b == c
        "#,
    )
    .unwrap();
}


#[test]
fn iterative_approach() {
    eval(
        r#"
            idx = 0
            const STRING = "Hello!"
        
            get_char = fn(idx: int) -> str? {
                if idx > 5 {
                    return nil
                }
        
                return STRING[idx]
            }

            result = ""
        
            while next ?= get_char(idx) {
                idx = idx + 1

                result = result + next * idx
            }

            assert result == "Heelllllllooooo!!!!!!"
        "#,
    )
    .unwrap();
}

#[test]
fn non_null_eval() {
    eval(
        r#"
            if a ?= 5 {
                # ...
            } else {
                assert false
            }
        "#,
    )
    .unwrap();
}


#[test]
#[should_panic = "use of undeclared variable"]
fn proper_scoping_if() {
    eval(
        r#"
            const SOME_VALUE: str? = "Hello!"

            if a ?= SOME_VALUE {
                # ...
            }

            assert a == "Hello!"
        "#,
    )
    .unwrap();
}

#[test]
#[should_panic = "use of undeclared variable"]
fn proper_scoping_while() {
    eval(
        r#"
            const SOME_VALUE: str? = "Hello!"

            while xyz ?= SOME_VALUE {
                # break
            }

            assert xyz == "Hello!"
        "#,
    )
    .unwrap();
}