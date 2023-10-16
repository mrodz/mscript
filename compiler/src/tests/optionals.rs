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