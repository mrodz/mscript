//! Constants used for instruction parsing.

use std::collections::HashMap;

use once_cell::sync::Lazy;

use super::instruction::implementations;
use super::instruction::InstructionSignature;

/// This constant maps an instruction's string representation to its byte identifier.
/// ie.
/// * "printn" -> 0x13
/// * "equ" -> 0x20
/// * ...
pub static REPR_TO_BIN: Lazy<HashMap<&[u8], u8>> = Lazy::new(|| {
    (0..BIN_TO_REPR.len())
        .map(|idx| (BIN_TO_REPR[idx], idx as u8))
        .collect()
});

/// This is the total tally of the instructions that the interpreter supports.
///
/// Saving this as a constant makes it harder for the arrays to fall out of sync
/// by requiring that they both take the same size.
pub const INSTRUCTION_COUNT: usize = 45;

/// This is an array that provides O(1) lookups of names from bytes.
pub static BIN_TO_REPR: [&[u8]; INSTRUCTION_COUNT] = [
    /* 0x00 [0]  */ b"nop",
    /* 0x01 [1]  */ b"constexpr",
    /* 0x02 [2]  */ b"stack_dump",
    /* 0x03 [3]  */ b"pop",
    /* 0x04 [4]  */ b"bin_op",
    /* 0x05 [5]  */ b"vec_op",
    /* 0x06 [6]  */ b"bool",
    /* 0x07 [7]  */ b"string",
    /* 0x08 [8]  */ b"bigint",
    /* 0x09 [9]  */ b"int",
    /* 0x0A [10] */ b"float",
    /* 0x0B [11] */ b"char", // @DEPRECATED
    /* 0x0C [12] */ b"byte",
    /* 0x0D [13] */ b"make_function",
    /* 0x0E [14] */ b"make_object",
    /* 0x0F [15] */ b"make_vector",
    /* 0x10 [16] */ b"void",
    /* 0x11 [17] */ b"breakpoint",
    /* 0x12 [18] */ b"ret",
    /* 0x13 [19] */ b"printn",
    /* 0x14 [20] */ b"call",
    /* 0x15 [21] */ b"call_object",
    /* 0x16 [22] */ b"stack_size",
    /* 0x17 [23] */ b"store",
    /* 0x18 [24] */ b"store_object",
    /* 0x19 [25] */ b"load",
    /* 0x1A [26] */ b"load_local",
    /* 0x1B [27] */ b"typecmp",
    /* 0x1C [28] */ b"if",
    /* 0x1D [29] */ b"jmp",
    /* 0x1E [30] */ b"endif", // @DEPRECATED
    /* 0x1F [31] */ b"strict_equ",
    /* 0x20 [32] */ b"equ",
    /* 0x21 [33] */ b"arg",
    /* 0x22 [34] */ b"mutate",
    /* 0x23 [35] */ b"load_callback", // Same as `load_object`
    /* 0x24 [36] */ b"load_object", // Same as `load_callback`
    /* 0x25 [37] */ b"call_lib",
    /* 0x26 [38] */ b"len",
    /* 0x27 [39] */ b"done",
    /* 0x28 [40] */ b"update",
    /* 0x29 [41] */ b"scope", // Same as `else`
    /* 0x2A [42] */ b"else", // Same as `scope`
    /* 0x2B [43] */ b"neg",
    /* 0x2C [44] */ b"neq",
];

/// Similar to [`BIN_TO_REPR`][crate::instruction_constants::BIN_TO_REPR],
/// except that this array contains function pointers to the evaluable instructions.
pub static FUNCTION_POINTER_LOOKUP: [InstructionSignature; INSTRUCTION_COUNT] = [
    implementations::nop,
    implementations::constexpr,
    implementations::stack_dump,
    implementations::pop,
    implementations::bin_op,
    implementations::vec_op,
    implementations::make_bool,
    implementations::make_str,
    implementations::make_bigint,
    implementations::make_int,
    implementations::make_float,
    implementations::nop, // @DEPRECATED
    implementations::make_byte,
    implementations::make_function,
    implementations::make_object,
    implementations::make_vector,
    implementations::void,
    implementations::breakpoint,
    implementations::ret,
    implementations::printn,
    implementations::call,
    implementations::call_object,
    implementations::stack_size,
    implementations::store,
    implementations::store_object,
    implementations::load,
    implementations::load_local,
    implementations::typecmp,
    implementations::if_stmt,
    implementations::jmp,
    implementations::nop, // @DEPRECATED
    implementations::strict_equ,
    implementations::equ,
    implementations::arg,
    implementations::mutate,
    implementations::load_callback, // Same as `load_object`
    implementations::load_callback, // Same as `load_callback`
    implementations::call_lib,
    implementations::len,
    implementations::done,
    implementations::update,
    implementations::else_stmt, // Same as `else`
    implementations::else_stmt, // Same as `scope`
    implementations::neg,
    implementations::neq,
];
