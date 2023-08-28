use std::rc::Rc;

use anyhow::{Result, bail, Context};

use crate::{Ctx, InstructionExitState, Instruction, Vector, variables::Primitive, stack::VariableFlags, rc_to_ref};

use serde_derive::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug)]
pub enum VecOpIndex {
	Direct(usize),
	FromName(Box<str>)
}

impl VecOpIndex {
	fn into_usize(self, context: &Ctx) -> Result<usize> {
		match self {
			Self::Direct(number) => Ok(number),
			Self::FromName(name) => {
				let (primitive, ..) = context.load_local(name.as_ref()).with_context(|| format!("{name} is not a name from this scope"))?.as_ref();
				let as_usize = primitive.try_into_numeric_index()?;
				Ok(as_usize)
			}
		}
	}
}

#[derive(Serialize, Deserialize, Debug)]
pub enum VecOp {
	QuickPushTo(Box<str>),
	Index(VecOpIndex),
	Reverse,
}

impl Instruction for VecOp {
	fn execute(&self, context: &mut Ctx) -> Result<InstructionExitState> {
		match self {
			Self::Reverse => {
				let Some(Vector(vector)) = context.get_last_op_item_mut() else {
					bail!("Cannot perform a vector operation on a non-vector")
				};

				let reference = Rc::get_mut(vector).context("could not get reference to vector")?;
				reference.reverse();
			}
			Self::QuickPushTo(vec_name) => {
				if context.stack_size() != 1 {
                    bail!("vec_op +push operations require only a single item on the operating stack")
                }

                let new_val = context.pop().unwrap();

                let mut primitive_with_flags: Rc<(Primitive, VariableFlags)> = context.load_local(vec_name).context("vector not found for pushing")?;

                let primitive_with_flags: &mut (Primitive, VariableFlags) = Rc::make_mut(&mut primitive_with_flags);

                let Primitive::Vector(ref mut vector) = primitive_with_flags.0 else {
                    bail!("not a vector, trying to push")
                };

                let vector: &mut Vec<Primitive> = rc_to_ref(vector);

                vector.push(new_val);
			}
			Self::Index(index_variant) => {
				let Some(indexable) = context.pop() else {
                    bail!("the stack is empty");
                };

				let index = index_variant.into_usize(context)?;

				match indexable {
                    Primitive::Vector(vector) => {
                        let r: *mut Primitive = rc_to_ref(&vector).get_mut(index).with_context(|| format!("index {index} out of bounds (len {})", vector.len()))?;
                        context.push(Primitive::HeapPrimitive(r));

                    }
                    Primitive::Str(string) => {
                        let mut str_chars = string.chars();
                        context.push(Primitive::Str(str_chars.nth(index).with_context(|| format!("index {index} out of bounds (len {} chars, {} bytes)", string.chars().count(), string.len()))?.to_string()))
                    }
                    _ => bail!("Cannot perform a vector operation on a non-vector"),
                }
			}
		}

		Ok(InstructionExitState::Processed)
	}
}