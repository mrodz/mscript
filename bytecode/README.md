# Bytecode
Here is the implementation of the bytecode interpreter. 

There are two forms of bytecode: Human-Readable Bytecode and the actual raw bytes. 

The first, Human-Readable Bytecode (HRB), can be used to write actual bytecode much faster than one could do with a Hex editor. There is CLI tooling to help transpile files (use the `.transpiled.mmm` extension for HRB) into actual bytecode (`.mmm` extension).

The idea of "bytecode" is really just a series of executable instructions. These instructions are grouped together under labels called functions. Certain bytecode instructions allow functions to jump to other functions, others might produce side effects, and more. A function can have exactly one return value.

When writing bytecode by hand (using HRB + transpilation), statements are separated by OS-independent newlines. The bytecode that actually gets executed by the interpreter has statements seperated by null bytes (0x00).

Example HRB format:
```
function main
	instruction_name arg1 arg2 "long arg"
	instruction_without_args
	...
end
```

The actual bytecode takes the following shape:
```
f function_name{NUL}{instruction_byte} arg1 arg2 "long arg"{NULL}...e{NULL}
```

## Hello World
To better explain how bytecode is laid out, let's look at the code for Hello World. This example can be found [here](../examples/bytecode/hello_world/).

In its human form, the code would be:
```
function main
	string "Hello World"
	printn *
end
```

In its raw instruction form:
```
f main{NUL}{BEL} "Hello World"{NUL}{DC3} *{NUL}e{NUL}
```

In a Hex Editor:
```
(1). 66 20 6D 61 69 6E 00 (2). 07 20 22 48 65 6C 6C 6F 20 57 6F 72 6C 22 00 (3). 13 20 2A 00 (4). 65 00
```
1. function main
2. string "Hello World"
3. printn *
4. end

# Interpreter Internals
When a file is opened for the first time, a parser will identify functions and group together instructions. Instructions store their identifying byte plus their arguments as a `String` slice on the heap. Once a file is opened, a lookup table of functions -> instructions for that unit will stay open for the lifetime of the interpreter. The handle to the file, though, will be freed after an initial read of its contents.

Primitives are wrappers around Rust's own datatypes. Most are copied when passed to functions and when shared by variables. Some, like Objects and Vectors, are reference-counted atomically.

Stack space allocated to the program can be controlled via a CLI flag (-X, --stack-size). The default is 4MB.

# Foreign Function Interface
Using dynamically-loaded libraries (.dll, .so, .lib, ...) files allows for interoperability between MScript and other languages. Currently, there is only support for libraries compiled from Rust, but stay tuned for C/C++ Header files.

## How to use this feature in bytecode:
Create a new crate. Its `Cargo.toml` should contain the following:

```toml
...

[lib]
crate-type = ["dylib"]

[dependencies]
bytecode = { path = "path/to/bytecode" }

...
```

Put your functions in the crate's lib.rs!
```rs
use bytecode::BytecodePrimitive;
use bytecode::{int, raise_error};
use bytecode::FFIReturnValue;

#[no_mangle]
pub fn adder(args: &[BytecodePrimitive]) -> FFIReturnValue {
    let (Some(BytecodePrimitive::Int(x)), Some(BytecodePrimitive::Int(y))) = (args.get(0), args.get(1)) else {
        raise_error!("cannot add two non-ints!")
    };

    let result = x + y;

    println!("hello from rust!!! the answer is {result}");

    FFIReturnValue::Value(int!(result))
}
```

The #[no_mangle] attribute is critical; otherwise, you might not be able to find your symbol.

Once this is set up, take a look at the `call_lib` function in the `Bytecode Instruction List`. You can also view this function in action [here](../ffi/).

# Terminology & Concepts
### Local Operating Stack  
The MScript Interpreter is a Stack Machine. To demonstrate, examine the following bytecode.
```
int 5     # stack = [5]
int 10    # stack = [5, 10]
bin_op +  # stack = [15]

printn *  # <- outputs "15"
```
### Path  
This is the way the interpreter stores absolute locations. Format:

```
/path/to/file/bytecode.mmm#function_name_here
```

Under the hood, the interpreter opens a read-only handle to the file and saves symbols (functions, consts, etc.).

### Instruction
A single line of bytecode. Format:

```
instruction_name args...
```
See `Bytecode Instruction List` for instruction documentation.

### Context (May be referred to as Ctx)  
Exposes program elements visible to an instruction. These include:
- the local operating stack
- the call stack
- a pointer to the function the instruction is located in
- functions to signal a return (and optionally a value)
- the ability to request a jump to another path


# Bytecode Instruction List:
Format: 
`name <! if fallible> [arg1, [v1_arg2 | v2_arg2], [arg3]?, 'constant string'] (==required_stack_items)`

---

### `constexpr ! [to_evaluate]`
Evaluate a constant expression to a type, and push it to stack.
```
constexpr "Hello" # pushes Str("Hello")
constexpr 4.5     # pushes Float(4.5_f64)
constexpr 4       # pushes Int(4_i32)
constexpr 0b101   # pushes Byte(0b101)
constexpr '@'     # pushes Char('@')
constexpr true    # pushes Bool(true)
```
If you already know the type of a constexpr beforehand, use its corresponding `{{type}}` instruction.

| ! | Reason |
| - | - |
| 1 | [to_evaluate] is not a invalid constexpr. |

---

### `stack_dump`
Useful for development; print the current local stack, call stack, and other useful info to stdout.

---

### `pop`
Pop the most recent item on the local stack.

---

### `bin_op ! ['+' | '-' | '*' | '/' | '%' | '>' | '>=' | '<' | '<=' | '=' | 'and' | 'or' | 'xor'] (==2)`
Perform fast arithmetic on two loaded items.

| ! | Reason |
| - | - |
| 1 | Local stack size is != 2. |
| 2 | Checked integer operation (i32, i128) results in / by zero. |
| 3 | Checked integer operation (i32, i128) results in an overflow. |
| 4 | Boolean operation on non-booleans. |
| 5 | Unimplemented operation. |

---

### `vec_op ! [vector_function, [arguments, ...]?]`
Perform native manipulation on a Vector primitive. Requires a function name, and optionally, arguments.

Available interfaces:
* `vec_op [idx]`  
Will retrieve an element at `idx`.
  ```
  int 1
  int 2
  int 3
  make_vec
  vec_op [1]
  printn *     > Outputs 2
  ``` 
* `vec_op reverse`  
Reverses the vector
* `vec_op mut idx`  
Mutate the vector by replacing the `Primitive` at `idx` with the item on top of the local operating stack.
  ```
  int 5
  int 10
  make_vec

  int 20
  vec_op mut 0
  printn *     > Outputs [20, 10]
  ```

| ! | Reason |
| - | - |
| 1 | Each vector function can fail uniquely. |

---

### `nop`
No operation.

---

### `bool ! [should_be_bool]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_bool] is not boolean. |

---

### `string ! [should_be_str]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_str] is not a string. |

---

### `int ! [should_be_int]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_int] is not an int. |

---

### `float ! [should_be_float]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_float] is not a float. |

---

### `byte ! [should_be_byte]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_byte] is not a byte. |

---

### `make_object`
Create an object by grouping all of the variables local to a scope; they are stored under the name of a function.

Objects can:
* be passed to functions
* be mutated (type safety checked at runtime)
* have methods associated to them

For example, the following code:

```
person = obj(name, age)
	.grow_up()
		age += 1

me = person("Mateo", 16)
me.grow_up()
```

Will compile into:

```
function person$grow_up
	load_object age
	int 1
	bin_op +
	store_object age

	void
	ret
end

function person
	arg 0
	store name
	arg 1
	store age

	make_object

	ret
end

function main
	string "Mateo"
	int 16
	call path/to/file.mmm#person
	store me

	load_local me
	call_object path/to/file.mmm#person$grow_up

	void
	ret
end
```

---

### `make_vector ! [[capacity]?]`
If capacity is present, will initialize a blank vector with a specified capacity.
Otherwise, will consume the local operating stack and copy its contents to a new vector.

| ! | Reason |
| - | - |
| 1 | [capacity] is not a usize. |

---

### `make_function ! [path_to_function, [callback_variable, ...]?]`
Make a function "pointer", from a path. The path is not checked until a user calls this pointer.

The primary usage of this instruction is to create callbacks, otherwise known as closures.

The following bytecode accomplishes the same as the python snippet below.

```
function add_n$0
	arg 0
	store x

	load_callback number
	load_local x

	bin_op +

	ret
end

function add_n
	arg 0
	store number

	make_function path/to/file.mmm#add_n$0 number

	ret
end

function main
	int 50
	call path/to/file.mmm#add_n
	store add_fifty

	int 75
	load add_fifty
	call

	printn *

	void
	ret
end
```

```py
def add_n(number):

	def result(x):
		return x + number

	return result

add_fifty = add_n(50)
print(add_fifty(75)) # Output: 125
```

By passing the names of registered variables, this callback will freeze their values and add them to a pool associated with the function pointer. These copies can later be "looked-up" using the `load_callback` instruction.   

| ! | Reason |
| - | - |
| 1 | Attempting to freeze a variable that does not exist. |

---

### `void`
Void all items in the local stack. In other words, it removes all items loaded in the stack.

---

### `breakpoint `
Stop program execution and display a minimal terminal menu with options to debug.

---

### `ret ! (==1)`
Return from a function. The caller will have access to the single variable in the stack when this was instruction ran. It is reachable via the function's Context.

| ! | Reason |
| - | - |
| 1 | Stack size != 1. |

---

### `printn ! ['*' | [stack_idx]]`
`'*'` will print every item in the stack, but if given an argument of type usize, will index into the stack at `stack_idx` instead and print what it finds.

| ! | Reason |
| - | - |
| 1 | [stack_idx] is out of bounds on the local stack. |

---

### `call ! [[path]?]`
Sends a request to call a function, given a path.

* If explicitly passed a path as an argument, the interpreter will immediately signal a jump request.
* Otherwise, will look at the last item on the local stack. If it is a function, will send a jump request.

If the file has not yet been loaded, will open the file and save a its handle. Otherwise, it will look at cached files and use that content. Files are cached on program startup.

If the interpreter accepts the request, and calling the function returns a value, it will get pushed on to the end of the local stack.

| ! | Reason |
| - | - |
| 1 | Argument length != 1 and there are no items in the local stack. |
| 2 | Attempting to call an item on the local stack that is not a function. |
| 3 | (_Indirect_) If the request to jump is accepted and the path is malformed. |

---

### `call_object ! [path] (>=1)`
Sends a request to call a method on an object, given a path.

Acts just like `call`, except that the first item on the local operating stack must be an Object. This parameter is not passed as an argument, but the calling function will have access to its fields and other methods. 

| ! | Reason |
| - | - |
| 1 | Argument length == 0. |
| 2 | First item on the local is not an Object. |
| 3 | [path] does not match one of object's methods. | 
| 4 | (_Indirect_) If the request to jump is accepted and the path is malformed. |

---

### `call_lib ! [lib_path] [func_name] (==?)`
Sends a request to call a method from a dynamically linked, given a path and its symbol name.

Acts just like `call`, except that the function is accessed via FFI.

One can create a function like this in a crate with the interpreter added as a dependency. Compile it as a [dylib](https://doc.rust-lang.org/reference/linkage.html) and save it in `test.dll`:

```rs
use bytecode::BytecodePrimitive;
use bytecode::{int, raise_error};
use bytecode::FFIReturnValue;

#[no_mangle]
pub fn adder(args: &[BytecodePrimitive]) -> FFIReturnValue {
    let (Some(BytecodePrimitive::Int(x)), Some(BytecodePrimitive::Int(y))) = (args.get(0), args.get(1)) else {
        raise_error!("cannot add two non-ints!")
    };

    let result = x + y;

    println!("hello from rust!!! the answer is {result}");

    FFIReturnValue::Value(int!(result))
}
```

To call the function, use the following bytecode:
```
function main
	int 5
	int 10
	call_lib "path/to/test.dll" adder
	printn * # will output 15!
end
```


| ! | Reason |
| - | - |
| 1 | The external function panics. |
| 2 | The external function raises an exception. |
| 3 | [lib_path] does not exist or cannot be opened as a library. | 
| 4 | [func_name] is not a symbol in [lib_path]. |

---

### `stack_size`
Will return the depth of the call stack, ie. how many function calls can be traced to the current executing instruction. The result of this operation gets pushed on to the end of the local stack.

---

### `store ! [name] (==1)`
Stores a variable to the current stack frame. Child frames will have access to its value.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |

---

### `store_object ! [name] (==1)`
Stores a variable to an object. The variable must have already been mapped, and the type of the item in the stack must equal the type of the previous object field. This is the way to mutate an object from within a function. For external modification, use the `mutate` instruction.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |
| 2 | Field does not exist on object |
| 3 | Type mismatch |

---

### `mutate ! [name] (==2)`
Update an object's fields. The first item in the stack must be the object, and the second the updated value.

| ! | Reason |
| - | - |
| 1 | Argument length != 2. |
| 2 | Field [name] does not exist on the object |
| 3 | Type mismatch |w

---

### `load ! [name]`
Loads a variable from the current stack frame. If not found, will trickle the search upwards.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |
| 2 | The variable hasn't been stored by any parent frames. |

---

### `load_local ! [name]`
Loads a variable from the current stack frame. If not found, the program will exit. This method is preferred when referencing local variables, as normal `load` calls can take a long time depending on how deeply-nested the stack frame is.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |
| 2 | The variable hasn't been stored by the current frame. |

---


### `load_callback ! [name]`
Loads a variable from the frozen callback pool. If not found, the program will exit. 

| ! | Reason |
| - | - |
| 1 | The variable hasn't been stored in the data pool. |

---

### `load_object ! [name]`

Equivalent to `load_callback`. This was introduced to make it clearer when you're dealing with objects.

| ! | Reason |
| - | - |
| 1 | The variable hasn't been stored in the data pool. |

---

### `if ! [falsey_jump_index] (>=1)`
This function will pop a boolean value from the top of the local stack. If truthy, this statement returns. If falsey, it will instruct the interpreter to jump to the Nth instruction in its function, as denoted by [falsey_jump_index].

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |
| 2 | Last stack item != Bool. |
| 3 | Argument[0] != usize. |

---

### `else !`
Similar to an 'else' in any other programming language. Will start executing if the previous `if` command did not evaluate to true.

| ! | Reason |
| - | - |
| 1 | The instruction points to a file position that is not a `Box<EndIf>`. |
| 2 | *panics!* The interpreter determines this `else` instruction is not contained within an `if` context.

---

### `endif !`
Closes an `if` instruction, then clears the local stack.

| ! | Reason |
| - | - |
| 1 | *panics!* The interpreter determines this `endif` instruction is not contained within an `if` context. |

---

### `strict_equ ! (==2)`
Test equality on two items on the local stack. 
* Two items are equal if they are of the same type and share the same value.

| ! | Reason |
| - | - |
| 1 | Local stack size is != 2. |

---

### `equ ! (==2)`
Test equality on two items on the local stack. 
* Two items are equal if they share the same value. This check transcends type bounds, ie. 0b101 == 5i32 == 5.0000 == 5i128.

| ! | Reason |
| - | - |
| 1 | Local stack size is != 2. |
| 2 | Invalid comparison, ie. String with Int |

---