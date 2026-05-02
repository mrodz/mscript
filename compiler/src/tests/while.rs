use crate::eval;

#[test]
fn while_never_runs() {
    eval(
        r#"
        count = 0
        while false {
            count += 1
        }
        assert count == 0
    "#,
    )
    .unwrap();
}

#[test]
fn nested_while() {
    eval(
        r#"
        result = 0
        i = 0
        while i < 3 {
            j = 0
            while j < 4 {
                j += 1
                if j == 3 {
                    break
                }
                result += 1
            }
            i += 1
        }
        # 3 outer iterations * 2 inner iterations each = 6
        assert result == 6
    "#,
    )
    .unwrap();
}

#[test]
fn while_accumulate_string() {
    eval(
        r#"
        result = ""
        i = 0
        while i < 5 {
            result += i
            i += 1
        }
        assert result == "01234"
    "#,
    )
    .unwrap();
}

#[test]
fn while_multiple_exits() {
    eval(
        r#"
        found = -1
        i = 0
        while i < 20 {
            i += 1
            if i % 3 == 0 && i % 5 == 0 {
                found = i
                break
            }
        }
        assert found == 15
    "#,
    )
    .unwrap();
}

#[test]
fn while_loop() {
    eval(
        r#"
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
    "#,
    )
    .unwrap();
}

#[test]
fn continue_skip() {
    eval(
        r#"
        i = 0
        while i < 5 {
            i = i + 1
            continue
        }

        assert i == 5
    "#,
    )
    .unwrap();
}

#[test]
fn break_skip() {
    eval(
        r#"
        i = 0
        while i < 5 {
            i = i + 1
            break
        }

        assert i == 1
    "#,
    )
    .unwrap();
}
