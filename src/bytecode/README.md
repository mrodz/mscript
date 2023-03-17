# Bytecode
Here is the implementation of the bytecode interpreter. The interpreter will start at a file and look for...

```
function main
	...
end
```

...to start executing. This is the sole entrypoint to the program.

## Bytecode instruction list:
Format: 
`name [arg1, [v1_arg2 | v2_arg2], [arg3]?, 'constant string'] (==required_stack_items)`
### `constexpr [to_evaluate]`
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
### `stack_dump`
Useful for development; print the current local stack, call stack, and other useful info to stdout.
### `pop`
Pop the most recent item on the local stack.
### `bin_op [+ | - | * | / | %] (==2)`
Perform fast arithmetic on two loaded items.
### `nop`
No operation.
### `bool [should_be_bool]`
See `constexpr`. The program will crash if the argument is not boolean.
### `string [should_be_str]`
See `constexpr`. The program will crash if the argument is not a string.
### `int [should_be_int]`
See `constexpr`. The program will crash if the argument is not an int.
### `float [should_be_float]`
See `constexpr`. The program will crash if the argument is not a float.
### `char [should_be_char]`
See `constexpr`. The program will crash if the argument is not a char.
### `byte [should_be_byte]`
See `constexpr`. The program will crash if the argument is not a byte.
### `void`
Void all items in the local stack. In other words, it removes all items loaded in the stack.
### `breakpoint `
Stop program execution and display a minimal terminal menu with options to debug.
### `ret (==1)`
Return from a function. The caller will have access to the single variable in the stack when this was instruction ran.
### `printn ['*' | [stack_idx]]`
`'*'` will print every item in the stack, but if given an argument of type usize, will index into the stack at `stack_idx` instead and print what it finds.
### `call [path]`
Call a function, given a path. Path syntax: 
```
/path/to/file/bytecode.mmm#function_name_here
```
If the file has not yet been loaded, will open the file and save a its handle. Otherwise, it will look at cached files and use that content. Files are cached on program startup.

If calling the function returns a value, it will get pushed on to the end of the local stack.

### `stack_size`
Will return the depth of the call stack, ie. how many function calls can be traced to the current executing instruction. The result of this operation gets pushed on to the end of the local stack.