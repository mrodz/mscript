use crate::eval;

#[test]
fn assert_true() {
    eval(
        r#"
		assert true
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic]
fn assert_false() {
    eval(
        r#"
		assert false
	"#,
    )
    .unwrap();
}
