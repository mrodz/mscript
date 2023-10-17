use crate::eval;

#[test]
#[should_panic = "unwrap this optional to use its value"]
fn type_mismatch() {
    eval(r#"
        give_name = fn(input: int) -> str? {
            if input == 42 {
                return "Mateo"
            } else {
                return nil
            }
        }
    
        maybe_me: str = give_name(42)
    "#).unwrap();
}

#[test]
fn optional_return_type() {
    eval(r#"
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
    "#).unwrap();
}