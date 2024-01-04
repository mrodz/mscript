//! # Optimal Test Command:
//! ```bash
//! cargo +nightly test -Zbuild-std --target x86_64-pc-windows-msvc -Zbuild-std-features=core/debug_refcell --all -- --exact --nocapture
//! ```

mod assertions;
mod class;
mod comments;
mod functions;
mod r#if;
mod math;
mod modify_edge_cases;
mod modules;
mod number_loop;
mod optionals;
mod types;
mod r#while;

#[macro_export]
macro_rules! assert_proper_eq_hash {
    ($lhs:expr, $rhs:expr) => {
        use std::{collections::hash_map::DefaultHasher as State, hash::Hasher};

        let mut state = State::new();
        $lhs.hash(&mut state);
        let lhs_hash = state.finish();

        let mut state = State::new();
        $rhs.hash(&mut state);
        let rhs_hash = state.finish();

        assert_eq!($lhs, $rhs);
        assert_eq!(lhs_hash, rhs_hash);
    };
}
