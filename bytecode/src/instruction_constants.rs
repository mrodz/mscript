//! Constants used for instruction parsing.

use std::collections::HashMap;

use once_cell::sync::Lazy;

/// This constant maps an instruction's string representation to its byte identifier.
/// ie.
/// * "printn" -> 0x13
/// * "equ" -> 0x20
/// * ...
pub static REPR_TO_BIN: Lazy<HashMap<&str, u8>> = Lazy::new(|| {
    (0..BIN_TO_REPR.len())
        .map(|idx| (BIN_TO_REPR[idx], idx as u8))
        .collect()
});

macro_rules! generate_consts {
    ($($const_name:ident $index:literal)+) => {
        #[macro_export]
        macro_rules! query {
            ($ctx:expr, $instruction:expr) => {
                match ($instruction).id {
                    $(
                        $index => casey::lower!{$crate::instruction::implementations::$const_name($ctx, &(($instruction).arguments))},
                    )+
                    bad => unreachable!("impossible op code: {bad}"),
                }
            };
        }

        mod query_export {
            pub use query;
        }

        pub(crate) use query_export::query;

        pub static BIN_TO_REPR: &[&str] = &[
            $(
                casey::lower!(stringify!($const_name)),
            )+
        ];

        pub mod id {
            type U8 = u8;
            $(
                casey::upper!{pub const $const_name: u8 = $index;}
            )+
        }
    };
}

generate_consts! {
    NOP                           0
    WHILE_LOOP                    1
    STACK_DUMP                    2
    POP                           3
    BIN_OP                        4
    VEC_OP                        5
    MAKE_BOOL                     6
    MAKE_STR                      7
    MAKE_BIGINT                   8
    MAKE_INT                      9
    MAKE_FLOAT                   10
    MAKE_BYTE                    11
    MAKE_FUNCTION                12
    MAKE_OBJECT                  13
    MAKE_VECTOR                  14
    VOID                         15
    BREAKPOINT                   16
    RET                          17
    PRINTN                       18
    CALL                         19
    CALL_OBJECT                  20
    STACK_SIZE                   21
    STORE                        22
    STORE_OBJECT                 23
    LOAD                         24
    LOAD_FAST                    25
    IF_STMT                      26
    JMP                          27
    EQU                          28
    ARG                          29
    MUTATE                       30
    LOAD_CALLBACK                31
    CALL_LIB                     32
    DONE                         33
    ELSE_STMT                    34
    NEG                          35
    NEQ                          36
    NOT                          37
    CALL_SELF                    38
    STORE_SKIP                   39
    FAST_REV2                    40
    JMP_POP                      41
    STORE_FAST                   42
    DELETE_NAME_SCOPED           43
    DELETE_NAME_REFERENCE_SCOPED 44
    PTR_MUT                      45
    ASSERT                       46
    RESERVE_PRIMITIVE            47
    LOOKUP                       48
    LD_SELF                      49
    EXPORT_NAME                  50
    EXPORT_SPECIAL               51
    LOAD_SELF_EXPORT             52
    UNWRAP_INTO                  53
    UNWRAP                       54
    JMP_NOT_NIL                  55
    BIN_OP_ASSIGN                56
    RET_MOD                      57
    MODULE_ENTRY                 58
    SPLIT_LOOKUP_STORE           59
    MAKE_MAP                     60
    FAST_MAP_INSERT              61
}
