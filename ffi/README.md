# Linkage example
This is a test crate that makes use of MScript's dynamic library loading.

Read the documentation of `bytecode/README.md` for more information.

A function can be called via FFI if it has a signature like this:
```rs
fn my_function(args: &[bytecode::BytecodePrimitive]) -> bytecode::FFIReturnValue {
	// ...
}
```

Errors **should not** be handled with panics! Use the helpful `raise_error!` macro instead.

```rs
use bytecode::raise_error;

// ...

// Uh oh! We hit a custom error.
raise_error!("That's no Moon!!!!")
```

To return a primitive variable to the interpreter, try using one of the shorthands:
```rs
use bytecode::{string, bigint, int, float, bool, byte, function, vector, object};

let s = string!(raw "Hello!")
let bI = bigint!(29992992929922);
let i = int!(5);
let f = float!(3.14159);
let b = bool!(false);
let B = byte!(0b101);
// ...

```

This crate is intentionally left out of the main crate's workspace because it is merely an example of what MScript's FFI can do.

If you have any questions, feel free to raise an Issue or start a Discussion.