use anyhow::{bail, Result, Context};

use crate::{Ctx, Instruction, InstructionExitState, bool, Bool, instruction_constants::BIN_OP_BYTE, SerializedInstructionBuilder, serialization::InstructionDeserializationFactory};

#[derive(Debug)]
pub enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
	Equals,
	BooleanAnd,
	BooleanOr,
	BooleanXor,
}

impl Instruction for BinOp {
    fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
        let (Some(right), Some(left)) = (context.pop(), context.pop()) else {
			bail!("bin_op requires two items on the local operating stack (found {:?})", context.get_local_operating_stack())
		};

        let left = left.move_out_of_heap_primitive();
        let right = right.move_out_of_heap_primitive();

		use BinOp::*;

        let result = match (self, left, right) {
            (Add, ..) => left + right,
            (Subtract, ..) => left - right,
            (Multiply, ..) => left * right,
            (Divide, ..) => left / right,
            (Modulo, ..) => left % right,
            (GreaterThan, ..) => Ok(bool!(left > right)),
            (LessThan, ..) => Ok(bool!(left < right)),
            (GreaterThanEquals, ..) => Ok(bool!(left >= right)),
            (LessThanEquals, ..) => Ok(bool!(left <= right)),
            (Equals, ..) => Ok(bool!(left.equals(&right)?)),
            (BooleanAnd, Bool(x), Bool(y)) => Ok(bool!(x && y)),
			(BooleanAnd, _, _) => bail!("BooleanAnd can only work with booleans (Found {left} && {right})"),
            (BooleanOr, Bool(x), Bool(y)) => Ok(bool!(x || y)),
			(BooleanOr, _, _) => bail!("BooleanOr can only work with booleans (Found {left} && {right})"),
            (BooleanXor, Bool(x), Bool(y)) => Ok(bool!(x ^ y)),
			(BooleanXor, _, _) => bail!("BooleanXor can only work with booleans (Found {left} && {right})"),
        }.context("invalid binary operation")?;

		context.clear_and_set_stack(result);

		Ok(InstructionExitState::Processed)
    }

    fn deserialize(bytes: &[u8]) -> Self {
        use BinOp::*;

        let mut factory = InstructionDeserializationFactory::new(bytes);

        let ty: u8 = factory.next().unwrap();
        match ty {
            1 => Add,
            2 => Subtract,
            3 => Multiply,
            4 => Divide,
            5 => Modulo,
            6 => LessThan,
            7 => GreaterThan,
            8 => LessThanEquals,
            9 => GreaterThanEquals,
	        10 => Equals,
	        11 => BooleanAnd,
	        12 => BooleanOr,
	        13 => BooleanXor,
            other => unreachable!("{other} not a bin_op, (1..=13) only")
        }
    }

    fn serialize(&self) -> Vec<u8> {
        use BinOp::*;
        let bin_op_id: u8 = match self {
            Add => 1,
            Subtract => 2,
            Multiply => 3,
            Divide => 4,
            Modulo => 5,
            LessThan => 6,
            GreaterThan => 7,
            LessThanEquals => 8,
            GreaterThanEquals => 9,
	        Equals => 10,
	        BooleanAnd => 11,
	        BooleanOr => 12,
	        BooleanXor => 13,
        };

        SerializedInstructionBuilder::new(BIN_OP_BYTE)
            .add(bin_op_id)
            .build()
    }
}
