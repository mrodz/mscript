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