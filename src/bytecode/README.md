# Bytecode
Here is the implementation of the bytecode interpreter. The interpreter will start at a file and look for...

```
function main
	...
end
```

...to start executing. This is the sole entrypoint to the program.

# Terminology & Concepts
### Local Operating Stack  
Bytecode is stack-based, and all operations go through this operating stack. Here is a visualization:

```
int 5     # stack = [5]
int 10    # stack = [5, 10]
bin_op +  # stack = [15]

printn *  # <- outputs "15"
```
### Path  
This is the way the interpreter stores locations. Format:

```
/path/to/file/bytecode.mmm#function_name_here
```

Under the hood, the interpreter opens a read-only handle to the file and saves symbols (functions, consts, etc.). The handle is kept for the lifetime of the running program.

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

### `bin_op ! ['+' | '-' | '*' | '/' | '%'] (==2)`
Perform fast arithmetic on two loaded items.

| ! | Reason |
| - | - |
| 1 | Local stack size is != 2. |
| 2 | Checked integer operation (i32, i128) results in / by zero. |
| 3 | Checked integer operation (i32, i128) results in an overflow. |

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

### `char ! [should_be_char]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_char] is not a char. |

---

### `byte ! [should_be_byte]`
See `constexpr`.

| ! | Reason |
| - | - |
| 1 | [should_be_byte] is not a byte. |

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

### `stack_size`
Will return the depth of the call stack, ie. how many function calls can be traced to the current executing instruction. The result of this operation gets pushed on to the end of the local stack.

---

### `store ! [name]`
Stores a variable to the current stack frame. Child frames will have access to its value.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |

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
| 2 | The variable hasn't been stored in the callback pool. |

---

### `if ! (>=1)`
Will enter an if-statement context for conditional jumping. Start by peeking at the last item on the local stack, unwrap expecting a boolean value. Sends a signal up to the interpreter sharing whether the expression evaluated truthily. Once done, will clear the local stack.

| ! | Reason |
| - | - |
| 1 | Argument length == 0. |
| 2 | Argument[0] != Bool. |
| 3 | *panics!* The interpreter cannot find an `else`/`endif` for this `if`.

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