use crate::eval;

#[test]
fn person_class() {
    eval(
        r#"
		class Person {
			first_name: str
			last_name: str
			age: int
	
			constructor(self, first_name: str, last_name: str, age: int) {
				self.first_name = first_name
				self.last_name = last_name
				self.age = 16
			}
	
			fn grow_up(self) {
				self.age = self.age + 1
			}	
	
			fn full_name(self) -> str {
				return self.first_name + " " + self.last_name
			}
		}
	
		me = Person("Mateo", "Rodriguez", 16)
	
		assert me.age == 16
		me.grow_up()
		assert me.age == 17
	
		assert me.full_name() == "Mateo Rodriguez"
	"#,
    )
    .unwrap()
}
