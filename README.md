# MScript
[![Rust CI](https://github.com/mrodz/mscript-lang/actions/workflows/rust.yml/badge.svg)](https://github.com/mrodz/mscript-lang/actions/workflows/rust.yml)

This passion project is a fully-fledged programming language that supports much of what developers seek in a language.
- [x] Classes
  ```ts
  class Person {
    first_name: str
    last_name: str
    age: int

    constructor(self, first_name: str, last_name: str, age: int) {
      self.first_name = first_name
      self.last_name = last_name
      self.age = age
    }

    fn grow_up(self) {
      self.age += 1
    }

    fn get_full_name(self) -> str {
      return self.first_name + " " + self.last_name
    }
  }
  ```
- [x] Language Interoperability
  ```rb
  my_static_variable: int = symbol "TEST" : "/my/rust_library.so"

  some_c_function: fn(int, int) -> int = symbol "add_numbers" : "/mathlib.so"

  
  ```
- [x] Closures
  ```ts
  counter = fn() -> [fn() -> int, fn(), fn()] {
    state = 0
    return [
      fn() -> int {
        return state
      },
      fn() {
        modify state = state + 1
      },
      fn() {
        modify state = state - 1
      }
    ]
  }

  [poll_count, inc, dec] = counter()

  assert poll_count() == 0
  inc()
  assert poll_count() == 1
  dec()
  dec()
  assert poll_count() == -1
  ```
- [x] Strong Typing
  ```ts
  who_is_cool = fn() -> [str...] {
    return ["Mateo", "Scout", "Tico"]
  }

  people: [int...] = who_is_cool() # ❌
  people: [str...] = who_is_cool() # ✔️

  me: bool = people[0] # ❌
  me: str = people[0] # ✔️

  from 1 to false step "A", bad {
    # ❌
  }

  from 1 to 100, number {
    print number # ✔️
  }
  ```

# How To Use
MScript has a build suite, `mscript.exe`, that can:
- **run** MScript code
- **compile** MScript code to bytecode
- **execute** Precompiled MScript bytecode
- **transpile** Human-readable bytecode to executable bytecode

# Errors
This programming language is in beta production. This means that some features might break during compilation or at runtime. 

If you encounter a bug in any stage of the MScript build process, please report it under the `Issues` tab in this repo!

An error could take the form of an explicit `panic!()`, which means MScript is acting in a bugged state that could not have been expected.

More commonly, an error will spawn from a bytecode instruction that cannot execute properly. The output of said occurence might look like:
```
******* MSCRIPT INTERPRETER FATAL RUNTIME ERROR *******
Stack Machine: [Int(5), HeapPrimitive(&"Hello")]

Call stack trace:
>> path/to/file.mmm#__fn0
 ^ path/to/another_file.mmm#Object_37::$constructor
 ^ __module__

Caused By:
  0. Reason #1
  1. Reason #2
  2. ...

Please report this at https://github.com/mrodz/mscript-lang/issues/new
```

# My Goal
Solve a _Hard_ Leetcode problem in my programming language! 😊
