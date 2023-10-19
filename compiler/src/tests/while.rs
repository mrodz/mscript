use crate::eval;

#[test]
fn while_loop() {
    eval(r#"
        i = 0

        random_call = fn() -> bool {
            temp = i == 20
            return temp
        }
    
        while i < 9 && !random_call() {
            print i
            i = i + 1
        }
    
        assert i == 9
    "#).unwrap();
}

#[test]
fn continue_skip() {
    eval(r#"
        i = 0
        while i < 5 {
            i = i + 1
            continue
        }

        assert i == 5
    "#).unwrap();
}

#[test]
fn break_skip() {
    eval(r#"
        i = 0
        while i < 5 {
            i = i + 1
            break
        }

        assert i == 1
    "#).unwrap();
}

