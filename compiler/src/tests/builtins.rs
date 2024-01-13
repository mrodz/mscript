use crate::eval;

#[test]
fn list_random() {
	eval(r#"
		a = fn() -> int { return 5 }

		assert [1, 2, 3, a()].len() == 4
	
		const x = [5]
	
		take_ints = fn(in: [int]) {
			assert in[0] == 5
			assert in.len() == 1
		}
	
		take_ints(x)
	
		x: [int...] = [1, 2, 3, 4, 5]
		
		assert x == [1, 2, 3, 4, 5]

		x.reverse()

		assert x == [5, 4, 3, 2, 1]
	
		assert x.inner_capacity() >= 5
	
		x.ensure_inner_capacity(100)
	
		assert x.inner_capacity() >= 105
	"#).unwrap();
}