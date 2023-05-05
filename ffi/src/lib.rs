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