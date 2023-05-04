use std::cmp::Ordering;

use crate::{
    apply_bool_bin_op_if_applicable,
    bytecode::variables::Primitive::{self},
};

impl std::cmp::PartialOrd for Primitive {
    // Required method
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }

    // Provided methods
    fn lt(&self, other: &Self) -> bool {
        apply_bool_bin_op_if_applicable!(self < other)
    }

    fn le(&self, other: &Self) -> bool {
        apply_bool_bin_op_if_applicable!(self <= other)
    }

    fn gt(&self, other: &Self) -> bool {
        apply_bool_bin_op_if_applicable!(self > other)
    }

    fn ge(&self, other: &Self) -> bool {
        apply_bool_bin_op_if_applicable!(self >= other)
    }
}

impl std::cmp::Eq for Primitive {}

impl std::cmp::Ord for Primitive {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            Ordering::Equal
        } else if self > other {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}
