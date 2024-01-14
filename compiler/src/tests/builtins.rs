use crate::eval;

#[test]
fn list_random() {
    eval(
        r#"
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
	"#,
    )
    .unwrap();
}

#[test]
fn open_list_type() {
    eval(
        r#"
		square_it = fn(in: int) -> int {
			return in * in
		}
		
		only_even = fn(in: int) -> bool {
			return in % 2 == 0
		}
		
		x = [3, 4, 5].map(square_it).filter(only_even)
		
		assert typeof x == "[int...]"
		assert typeof x[0] == "int"
		
		assert x.len() == 1
		assert x == [16]
		assert x.inner_capacity() >= 1

		x.push(100)
		x.push(9999999)

		assert x == [16, 100, 9999999]
		assert x.remove(1) == 100
		assert x == [16, 9999999]

		x.push(5)
		
		type numberz int
		AAA: numberz = 60
		x.push(AAA)
		
		assert x == [16, 9999999, 5, 60]

		assert typeof x == "[int...]"
		assert typeof x[3] == "int"

		x = x.map(fn(in: int) -> str {
			return in + "!"
		})

		assert typeof x == "[str...]"

		assert x == ["16!", "9999999!", "5!", "60!"]
		
		x = x.join(["hello", "world"])

		assert typeof x[0] == "str"

		assert x == ["16!", "9999999!", "5!", "60!", "hello", "world"]
	"#,
    )
    .unwrap();
}

#[test]
#[should_panic = "this property does not exist on `[str, int, bool]`"]
fn push_to_mixed() {
    eval(
        r#"
		const tuple = ["message", 16, true]
		tuple.push(5)
	"#,
    )
    .unwrap();
}

#[test]
fn index_of() {
    eval(
        r#"
		x: [str...] = ["Nico", "Andreina", "Ignacio", "Mateo", "Marcus", "Ryan"]
		assert typeof x == "[str...]"

		j = x.index_of("Mateo")
		q = x.index_of("Bert")

		assert typeof j == "int?"
		assert typeof q == "int?"

		assert j == 3
		assert q == nil
	"#,
    )
    .unwrap();
}

#[test]
fn fn_is_closure() {
    eval(
        r#"
		outside = 5

		assert !fn() {
			print 5
		}.is_closure()

		assert fn() {
			print outside
		}.is_closure()

		a = fn() {
			outside
		}

		b = a

		assert a.is_closure() == b.is_closure() == true
		assert a.to_str() == b.to_str()

	"#,
    )
    .unwrap()
}
