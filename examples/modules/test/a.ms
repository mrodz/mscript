import type Foo, type Bar, Fizz from b

wooza: Foo = Fizz(42)
const num: Bar = "hello"

accept_string_like = fn(input: str) -> str {
  return input + "!"
}

assert accept_string_like(num) == "hello!"
assert accept_string_like("basic") == "basic!"