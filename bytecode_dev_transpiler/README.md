# Library to transpile bytecode
Developers might want to write some bytecode to visualize a feature or solve a compilation problem. However, writing bytecode by hand is:
* slow
* painful
* error-prone
* _insert negative adjective here xD_

This crate exposes functions that make writing raw bytecode a walk in the park. Say goodbye to Hex Editors!

All the rules of legacy bytecode have been preserved. Instructions written in text form are mapped to their corresponding binary values using the lookup table in [instruction_constants.rs](../bytecode/src/instruction_constants.rs).

See the documentation in the README file of the `/bytecode` crate for a list of bytecode instructions.