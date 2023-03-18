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

### `call ! [path]`
Sends a request to call a function, given a path.   

If the file has not yet been loaded, will open the file and save a its handle. Otherwise, it will look at cached files and use that content. Files are cached on program startup.

If the interpreter accepts the request, and calling the function returns a value, it will get pushed on to the end of the local stack.

| ! | Reason |
| - | - |
| 1 | Argument length != 1. |
| 2 | (_Indirect_) If the request to jump is accepted and the path is malformed. |

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