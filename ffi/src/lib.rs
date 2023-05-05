use bytecode::BytecodePrimitive;
use bytecode::int;
use bytecode::FFIReturnValue;
use std::error::Error;

#[no_mangle]
pub fn adder(args: &[BytecodePrimitive]) -> Result<FFIReturnValue, Box<dyn Error>> {
    let (BytecodePrimitive::Int(x), BytecodePrimitive::Int(y)) = (&args[0], &args[1]) else {
        return Err("cannot add two non-ints!".into())
    };

    let result = x + y;

    println!("hello from rust!!! the answer is {result}");

    Ok(FFIReturnValue(Some(int!(result))))
}