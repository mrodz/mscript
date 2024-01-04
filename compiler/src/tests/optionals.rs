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

            b: int? = nil
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

            b: int? = nil

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

            next: str? = nil
        
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
            a: int? = nil

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
fn proper_scoping_if() {
    eval(
        r#"
            const SOME_VALUE: str? = "Hello!"

            a: str? = nil

            if a ?= SOME_VALUE {
                # ...
            }

            assert a == "Hello!"
        "#,
    )
    .unwrap();
}

#[test]
fn proper_scoping_while() {
    eval(
        r#"
            const SOME_VALUE: str? = "Hello!"

            xyz: str? = nil

            while xyz ?= SOME_VALUE {
                break
            }

            assert xyz == "Hello!"
        "#,
    )
    .unwrap();
}

#[test]
fn get_keyword() {
    eval(
        r#"

        coolest_person: str? = nil

        const LIKES_GREEN = true
        const CAN_CODE = true
        const CAN_COOK = true

        if LIKES_GREEN && CAN_CODE && CAN_COOK {
            coolest_person = "Mom"
        }

        name: str = get coolest_person
    
        assert name == "Mom"
        assert coolest_person == name
    "#,
    )
    .unwrap();
}

#[test]
fn get_or_simple() {
    eval(
        r#"
        x = get nil or 5
        assert x == 5
    "#,
    )
    .unwrap();
}

#[test]
fn get_or_complex() {
    eval(
        r#"
        give_booze = fn(age: int) -> str? {
            if age < 21 {
                return nil
            }

            return "Moonshine"
        }

        first_label = get (give_booze(16)) or "Oops! You're underage"
        assert first_label == "Oops! You're underage"

        first_label = get (give_booze(55)) or "shhh this will never emerge"
        assert first_label == "Moonshine"
        
    "#,
    )
    .unwrap();
}

#[test]
#[should_panic = "could not get the type"]
fn abort_store_missing_type() {
    eval(
        r#"
        y = get nil
    "#,
    )
    .unwrap();
}

#[test]
#[should_panic = "Interpreter crashed"]
fn abort_store() {
    eval(
        r#"
        empty: bool? = nil

        y = get empty
    "#,
    )
    .unwrap();
}

#[test]
#[should_panic = "Interpreter crashed"]
fn abort_declaration() {
    eval(
        r#"
        get nil
    "#,
    )
    .unwrap();
}

#[test]
fn lazy_eval() {
    eval(
        r#"
        die = fn() -> str {
            assert false
            return "never"
        }

        get "liberty" or die()
    "#,
    )
    .unwrap();
}

#[test]
#[should_panic]
fn abort_in_or() {
    eval(
        r#"
        die = fn() {
            assert false
        }

        get nil or die()
    "#,
    )
    .unwrap();
}

#[test]
#[should_panic = "The `or` portion of this unwrap must yield `int`, but `int?` was found"]
fn type_safety_get_keyword() {
    eval(
        r#"
        empty_int: int? = nil
        another_empty_int: int? = nil

        get empty_int or another_empty_int
    "#,
    )
    .unwrap();
}
