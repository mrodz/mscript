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

#[test]
fn string_properties() {
    eval(
        r#"
        const MESSAGE = "hello world"

        assert MESSAGE[6] == "w"
        assert MESSAGE.len() == 11
        assert MESSAGE.substring(6, MESSAGE.len()) == "world"
        assert MESSAGE.contains("lo w")
        assert !MESSAGE.contains("a")
        assert MESSAGE.index_of("wo") == 6
        assert MESSAGE.index_of("ow") == nil
        assert typeof MESSAGE.index_of("ow") == "int?"
        assert MESSAGE.inner_capacity() >= 11
        assert MESSAGE.reverse() == "dlrow olleh"
        assert MESSAGE.insert(", you are my", 5) == "hello, you are my world"
        assert MESSAGE.replace(" world", "!") == "hello!"
        assert MESSAGE.delete(5, 7) == "helloorld"
        assert "25".parse_int() == 25
        assert "-25".parse_int() == -25
        assert "25.0".parse_int() == nil
        assert "twenty five".parse_int() == nil
        assert typeof "".parse_int() == "int?"
        assert "25".parse_int_radix(16) == 0x25
		assert "25".parse_int_radix(16) != 25
        assert "25".parse_int_radix(10) == 25
        assert "25".parse_bigint() == B25
        assert "-25".parse_bigint() == -B25
        assert "25.0".parse_bigint() == nil
        assert typeof "".parse_bigint() == "bigint?"
        assert "twenty five".parse_bigint() == nil
        assert "25".parse_bigint_radix(16) == 0x25
        assert "25".parse_bigint_radix(10) == B25
        assert get "true".parse_bool()
        assert !(get "false".parse_bool())
        assert "yes".parse_bool() == nil
        assert "3.14159".parse_float() == 3.14159
        assert "xyz".parse_float() == nil
        assert typeof "".parse_float() == "float?"
        assert "3".parse_byte() == 0b11
        assert "0b101".parse_byte() == 5
        assert "goodwill".split(4).to_str() == "[\"good\", \"will\"]"
        assert "goodwill".split(100).to_str() == "[\"goodwill\", \"\"]"
        assert "goodwill".split(-1).to_str() == "[\"goodwill\", \"\"]"
    "#,
    )
    .unwrap();
}

#[test]
fn number_properties() {
    eval(
        r#"
        assert 5.pow(2) == 25
        assert typeof 5.pow(2) == "bigint"
        assert typeof 5f.pow(2) == "float"
        assert 0b101.pow(2) == 25
        assert 5.pow(0) == 1
        assert 5.pow(-0.001.to_int()) == 1
        assert 25.powf(0.5) == 5
        assert 0x51.powf(1/2f) == 9
        assert 90.to_byte().to_ascii() == "Z"
        assert 3.1415.ipart() == 3
        assert (-5).abs() == (5).abs()
        assert (-2.718281828).abs() == (2.718281828).abs()
        assert (-0xFFFF).abs() == (-B65535).abs()
        assert 49.sqrt() == 49.powf(1f/2)
        assert 3.5.round() == 4
        assert 3.49999.round() == 3
        assert 3.5.ceil() == 4
        assert 3.5.floor() == 3

        type Numberz float
        const PI: Numberz = 3.14159265359

		assert PI.round() == 3
    "#,
    )
    .unwrap();
}
