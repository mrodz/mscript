# My attempt at a programming language!
This will be a compiled and interpreted language. In other words, one can compile source code to an intermediary bytecode format, which is then interpeted.


# Brainstorming syntax/Goals

The goal is for minimalistic and easy-to-learn syntax, inspired by Ruby and Python. The language should also be **useful** and easily-integratable into existing systems.

## Functions
```
factorial = fn(x)
	result = 1

	n from 1 to x
		result *= n

	return result

assert factorial(5) == 120
```

## OOP
```
person = obj(name, age)
	.speak = fn()
		print("hi! I'm", name)
	
	.grow_up = fn()
		name += 1

bob = person("Bob", 45)
bob.speak()
bob.grow_up()
```

## FFI
```
link "/path/to/libusb.dll" | "/path/to/libusb.so"

use std::native::null_ptr

libusb_init_context = linked fn(ctx, options, num_options)

pub libusb = obj()
	.init()
		libusb_init_context(null_ptr, null_ptr, null_ptr)
		print("Calling a C library via linker!")
```
