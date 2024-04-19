//! Everything to do with functions is found here:
//! * Execution of loaded bytecode functions ([`Function`])
//! * Representation of runtime callbacks/closures ([`PrimitiveFunction`])

use anyhow::{bail, Context, Result};
use gc::{Finalize, Trace};
use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::path::Path;
use std::rc::{Rc, Weak};

use crate::compilation_bridge::raw_byte_instruction_to_string_representation;
use crate::context::{Ctx, SpecialScope};
use crate::file::MScriptFile;
use crate::instruction::{Instruction, JumpRequestDestination};
use crate::instruction_constants::query;
use crate::{vector, GcVector};

use super::instruction::JumpRequest;
use super::stack::{Stack, VariableMapping};
use super::variables::Primitive;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BuiltInFunction {
    VecLen,
    VecReverse,
    VecInnerCapacity,
    VecEnsureInnerCapacity,
    VecMap,
    VecFilter,
    VecRemove,
    VecPush,
    VecJoin,
    VecIndexOf,
    FnIsClosure,
    GenericToStr,
    StrLen,
    StrSubstring,
    StrContains,
    StrIndexOf,
    StrInnerCapacity,
    StrReverse,
    StrInsert,
    StrReplace,
    StrDelete,
    StrParseInt,
    StrParseIntRadix,
    StrParseBigint,
    StrParseBigintRadix,
    StrParseBool,
    StrParseFloat,
    StrParseByte,
    StrSplit,
    GenericPow,
    GenericPowf,
    GenericSqrt,
    GenericToInt,
    GenericToBigint,
    GenericToByte,
    GenericToFloat,
    GenericAbs,
    ByteToAscii,
    FloatFPart,
    FloatIPart,
    FloatRound,
    FloatFloor,
    FloatCeil,
    MapLen,
    MapHasKey,
    MapReplace,
    MapKeys,
    MapValues,
    MapPairs,
}

type BuiltInFunctionReturnBundle = (
    Option<Primitive>,
    Option<Box<dyn RuntimeExecutionBridgeNotifier>>,
);

impl BuiltInFunction {
    pub fn run(&self, ctx: &mut Ctx) -> Result<BuiltInFunctionReturnBundle> {
        let mut arguments = ctx.ref_clear_local_operating_stack();

        for argument in arguments.iter_mut() {
            *argument = argument.clone().move_out_of_heap_primitive()?;
        }

        match self {
            Self::GenericToStr => {
                let Some(primitive) = arguments.first() else {
                    unreachable!();
                };

                Ok((Some(Primitive::Str(primitive.to_string())), None))
            }
            Self::VecLen => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let len = v.0.borrow().len();

                Ok((
                    Some(Primitive::Int(len.try_into().with_context(|| {
                        format!("vector len `{len}` could not fit in an int (i32)")
                    })?)),
                    None,
                ))
            }
            Self::VecReverse => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                v.0.borrow_mut().reverse();

                Ok((None, None))
            }
            Self::VecInnerCapacity => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let cap = v.0.borrow().capacity();

                Ok((
                    Some(Primitive::Int(cap.try_into().with_context(|| {
                        format!("vector capacity `{cap}` could not fit in an int (i32)")
                    })?)),
                    None,
                ))
            }
            Self::VecEnsureInnerCapacity => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(size)) = arguments.get(1) else {
                    unreachable!()
                };

                v.0.borrow_mut()
                    .reserve((*size).try_into().with_context(|| {
                        format!("additional vector capacity `{size}` could not fit in an int (i32)")
                    })?);

                Ok((None, None))
            }
            Self::VecMap => {
                let Primitive::Function(ref callback_fn) = arguments.remove(1) else {
                    unreachable!()
                };

                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                #[derive(Debug)]
                struct MapOp {
                    callback_path: String,
                    callback_state: Option<VariableMapping>,
                    underlying: GcVector,
                    call_stack: Rc<RefCell<Stack>>,
                    map_result: GcVector,
                    index: Cell<i32>,
                }

                impl MapOp {
                    fn new(
                        callback_fn: PrimitiveFunction,
                        underlying: GcVector,
                        call_stack: Rc<RefCell<Stack>>,
                    ) -> Self {
                        let underlying_len = { underlying.0.borrow().len() };
                        Self {
                            callback_path: callback_fn.location.clone(),
                            callback_state: callback_fn.callback_state.clone(),
                            call_stack,
                            map_result: GcVector::with_capacity(underlying_len),
                            underlying,
                            index: Cell::new(0),
                        }
                    }
                }

                impl RuntimeExecutionBridgeNotifier for MapOp {
                    fn wait_for(&self) -> Result<JumpRequest> {
                        let this_index = self.index.get();
                        self.index.set(this_index + 1);
                        let underlying = self.underlying.0.borrow();
                        let this_value: Primitive = underlying[this_index as usize].clone();

                        Ok(JumpRequest {
                            destination: JumpRequestDestination::Standard(
                                self.callback_path.clone(),
                            ),
                            arguments: vec![this_value],
                            callback_state: self.callback_state.clone(),
                            stack: self.call_stack.clone(),
                        })
                    }

                    fn then(&self, return_value: ReturnValue) -> Result<bool> {
                        let mut result = self.map_result.0.borrow_mut();

                        if let ReturnValue::Value(value) = return_value {
                            result.push(value);
                        }

                        Ok((self.index.get() as usize) < self.underlying.0.borrow().len())
                    }

                    fn finish(&self) -> Result<Option<Primitive>> {
                        Ok(Some(Primitive::Vector(self.map_result.clone())))
                    }
                }

                Ok((
                    None,
                    Some(Box::new(MapOp::new(
                        callback_fn.clone(),
                        v.clone(),
                        ctx.rced_call_stack(),
                    ))),
                ))
            }
            Self::VecFilter => {
                let Primitive::Function(ref callback_fn) = arguments.remove(1) else {
                    unreachable!()
                };

                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                #[derive(Debug)]
                struct FilterOp {
                    callback_path: String,
                    callback_state: Option<VariableMapping>,
                    underlying: GcVector,
                    call_stack: Rc<RefCell<Stack>>,
                    filter_result: GcVector,
                    index: Cell<i32>,
                }

                impl FilterOp {
                    fn new(
                        callback_fn: PrimitiveFunction,
                        underlying: GcVector,
                        call_stack: Rc<RefCell<Stack>>,
                    ) -> Self {
                        Self {
                            callback_path: callback_fn.location.clone(),
                            callback_state: callback_fn.callback_state.clone(),
                            call_stack,
                            filter_result: GcVector::default(),
                            underlying,
                            index: Cell::new(0),
                        }
                    }
                }

                impl RuntimeExecutionBridgeNotifier for FilterOp {
                    fn wait_for(&self) -> Result<JumpRequest> {
                        let this_index = self.index.get();
                        self.index.set(this_index + 1);
                        let underlying = self.underlying.0.borrow();
                        let this_value: Primitive = underlying[this_index as usize].clone();

                        Ok(JumpRequest {
                            destination: JumpRequestDestination::Standard(
                                self.callback_path.clone(),
                            ),
                            arguments: vec![this_value],
                            callback_state: self.callback_state.clone(),
                            stack: self.call_stack.clone(),
                        })
                    }

                    fn then(&self, return_value: ReturnValue) -> Result<bool> {
                        let mut result = self.filter_result.0.borrow_mut();

                        if let ReturnValue::Value(Primitive::Bool(true)) = return_value {
                            let underlying = self.underlying.0.borrow();
                            let this_index: usize = (self.index.get() - 1).try_into()?;
                            result.push(underlying[this_index].clone());
                        }

                        Ok(<i32 as TryInto<usize>>::try_into(self.index.get())?
                            < self.underlying.0.borrow().len())
                    }

                    fn finish(&self) -> Result<Option<Primitive>> {
                        Ok(Some(Primitive::Vector(self.filter_result.clone())))
                    }
                }

                Ok((
                    None,
                    Some(Box::new(FilterOp::new(
                        callback_fn.clone(),
                        v.clone(),
                        ctx.rced_call_stack(),
                    ))),
                ))
            }
            Self::VecRemove => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(i)) = arguments.get(1) else {
                    unreachable!()
                };

                let removed = v.0.borrow_mut().remove((*i).try_into().with_context(|| {
                    format!("vector index `{i}` could not fit in an int (i32)")
                })?);

                Ok((Some(removed), None))
            }
            Self::VecPush => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let Some(primitive) = arguments.get(1) else {
                    unreachable!()
                };

                v.0.borrow_mut().push(primitive.clone());

                Ok((None, None))
            }
            Self::VecJoin => {
                let Some(Primitive::Vector(v_original_shared)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Vector(v_add)) = arguments.get(1) else {
                    unreachable!()
                };

                {
                    let mut v_original = v_original_shared.0.borrow_mut();
                    let mut v_add = v_add.0.borrow_mut();

                    v_original.append(v_add.as_mut());
                }

                Ok((Some(Primitive::Vector(v_original_shared.clone())), None))
            }
            Self::VecIndexOf => {
                let Some(Primitive::Vector(v)) = arguments.first() else {
                    unreachable!()
                };

                let Some(primitive) = arguments.get(1) else {
                    unreachable!()
                };

                let view = v.0.borrow();

                let result = view.iter().enumerate().find(|(_, x)| {
                    x.equals(primitive)
                        .expect("the compiler allowed an illegal type comparison")
                });

                if let Some((result, _)) = result {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Int(
                            result.try_into().with_context(|| format!("vector index of element `{result}` could not fit in an int (i32)"))?,
                        ))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::FnIsClosure => match arguments.first() {
                Some(Primitive::Function(f)) => {
                    Ok((Some(Primitive::Bool(f.callback_state.is_some())), None))
                }
                Some(Primitive::BuiltInFunction(..)) => Ok((Some(Primitive::Bool(false)), None)),
                _ => unreachable!(),
            },
            Self::StrLen => {
                let Some(Primitive::Str(v)) = arguments.first() else {
                    unreachable!()
                };

                let len = v.len();

                Ok((
                    Some(Primitive::Int(len.try_into().with_context(|| {
                        format!("string length `{len}` could not fit in an int (i32)")
                    })?)),
                    None,
                ))
            }
            Self::StrSubstring => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(bottom)) = arguments.get(1) else {
                    unreachable!()
                };

                let Some(Primitive::Int(top)) = arguments.get(2) else {
                    unreachable!()
                };

                let bottom: usize = (*bottom).try_into().with_context(|| {
                    format!("bottom vector index `{bottom}` could not be used to index (usize)")
                })?;
                let top: usize = (*top).try_into().with_context(|| {
                    format!("top vector index `{top}` could not be used to index (usize)")
                })?;

                Ok((Some(Primitive::Str(s[bottom..top].to_owned())), None))
            }
            Self::StrContains => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Str(o)) = arguments.get(1) else {
                    unreachable!()
                };

                Ok((Some(Primitive::Bool(s.contains(o))), None))
            }
            Self::StrIndexOf => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Str(o)) = arguments.get(1) else {
                    unreachable!()
                };

                if let Some(start) = s.find(o) {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Int(
                            start.try_into().with_context(|| format!("index of found string pattern `{start}` could not fit in an int (i32)"))?,
                        ))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrInnerCapacity => {
                let Some(Primitive::Str(v)) = arguments.first() else {
                    unreachable!()
                };

                Ok((
                    Some(Primitive::Int(v.capacity().try_into().with_context(
                        || {
                            format!(
                                "string capacity `{}` could not fit in an int (i32)",
                                v.capacity()
                            )
                        },
                    )?)),
                    None,
                ))
            }
            Self::StrReverse => {
                let Some(Primitive::Str(v)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Str(v.chars().rev().collect())), None))
            }
            Self::StrInsert => {
                let Some(Primitive::Str(original)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Str(new)) = arguments.get(1) else {
                    unreachable!()
                };

                let Some(Primitive::Int(bottom)) = arguments.get(2) else {
                    unreachable!()
                };

                let mut result = original.clone();

                result.insert_str(
                    (*bottom).try_into().with_context(|| {
                        format!(
                            "string insertion index `{bottom}` could not be used to index (usize)"
                        )
                    })?,
                    new,
                );
                Ok((Some(Primitive::Str(result)), None))
            }
            Self::StrReplace => {
                let Some(Primitive::Str(original)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Str(pattern)) = arguments.get(1) else {
                    unreachable!()
                };

                let Some(Primitive::Str(replacement)) = arguments.get(2) else {
                    unreachable!()
                };

                Ok((
                    Some(Primitive::Str(original.replace(pattern, replacement))),
                    None,
                ))
            }
            Self::StrDelete => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(bottom)) = arguments.get(1) else {
                    unreachable!()
                };

                let Some(Primitive::Int(top)) = arguments.get(2) else {
                    unreachable!()
                };

                let bottom: usize = (*bottom).try_into().with_context(|| {
                    format!("string bottom index `{bottom}` could not be used to index (usize)")
                })?;
                let top: usize = (*top).try_into().with_context(|| {
                    format!("string bottom index `{top}` could not be used to index (usize)")
                })?;

                let start = top - bottom + 1;

                let mut result = String::with_capacity(s.len() - start);

                result.push_str(&s[..bottom]);
                result.push_str(&s[top..]);

                Ok((Some(Primitive::Str(result)), None))
            }
            Self::StrParseInt => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let s = if s.starts_with("0x") {
                    s.get(2..).unwrap_or_default()
                } else {
                    s
                };

                if let Ok(num) = s.parse::<i32>() {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Int(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseBigint => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let s = if s.starts_with("0x") {
                    s.get(2..).unwrap_or_default()
                } else {
                    s
                };

                if let Ok(num) = s.parse::<i128>() {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::BigInt(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseIntRadix => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(radix)) = arguments.get(1) else {
                    unreachable!()
                };

                let s = if s.starts_with("0x") {
                    s.get(2..).unwrap_or_default()
                } else {
                    s
                };

                if let Ok(num) = i32::from_str_radix(
                    s,
                    (*radix)
                        .try_into()
                        .with_context(|| format!("`{radix}` is an invalid radix"))?,
                ) {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Int(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseBigintRadix => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(radix)) = arguments.get(1) else {
                    unreachable!()
                };

                let s = if s.starts_with("0x") {
                    s.get(2..).unwrap_or_default()
                } else {
                    s
                };

                if let Ok(num) = i128::from_str_radix(
                    s,
                    (*radix)
                        .try_into()
                        .with_context(|| format!("`{radix}` is an invalid radix"))?,
                ) {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::BigInt(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseBool => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                if let Ok(b) = s.parse::<bool>() {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Bool(b))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseFloat => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                if let Ok(num) = s.parse::<f64>() {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Float(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrParseByte => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let (s, radix) = if s.starts_with("0b") {
                    (s.get(2..).unwrap_or_default(), 2)
                } else {
                    (s.as_str(), 10)
                };

                if let Ok(num) = u8::from_str_radix(s, radix) {
                    Ok((
                        Some(Primitive::Optional(Some(Box::new(Primitive::Byte(num))))),
                        None,
                    ))
                } else {
                    Ok((Some(Primitive::Optional(None)), None))
                }
            }
            Self::StrSplit => {
                let Some(Primitive::Str(s)) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(mid)) = arguments.get(1) else {
                    unreachable!()
                };

                if *mid < 0 {
                    return Ok((
                        Some(vector![
                            Primitive::Str(s.to_owned()),
                            Primitive::Str("".to_owned()),
                        ]),
                        None,
                    ));
                }

                if *mid
                    >= s.len()
                        .try_into()
                        .context("len (usize) does not fit in an int")?
                {
                    return Ok((
                        Some(vector![
                            Primitive::Str(s.to_owned()),
                            Primitive::Str("".to_owned()),
                        ]),
                        None,
                    ));
                }

                let (lhs, rhs) = s.split_at(
                    (*mid)
                        .try_into()
                        .with_context(|| format!("`{mid}` is an invalid index (usize)"))?,
                );

                Ok((
                    Some(vector![
                        Primitive::Str(lhs.to_owned()),
                        Primitive::Str(rhs.to_owned()),
                    ]),
                    None,
                ))
            }
            Self::GenericPow => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Int(power)) = arguments.get(1) else {
                    unreachable!();
                };

                if let Primitive::Float(f64) = this {
                    return Ok((Some(Primitive::Float(f64.powi(*power))), None));
                }

                let power_non_fp: u32 = (*power).try_into().with_context(|| {
                    format!("`{power}` is an invalid power for int bases (valid >= 0)")
                })?;

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::BigInt(i32.pow(power_non_fp) as i128),
                    Primitive::BigInt(i128) => Primitive::BigInt(i128.pow(power_non_fp)),
                    Primitive::Byte(u8) => Primitive::BigInt(u8.pow(power_non_fp) as i128),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericPowf => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let Some(Primitive::Float(power)) = arguments.get(1) else {
                    unreachable!();
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Float(f64::from(*i32).powf(*power)),
                    Primitive::BigInt(i128) => {
                        Primitive::Float(f64::from(*i128 as i32).powf(*power))
                    }
                    Primitive::Byte(u8) => Primitive::Float((*u8 as f64).powf(*power)),
                    Primitive::Float(f64) => Primitive::Float(f64.powf(*power)),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericSqrt => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Float(f64::from(*i32).sqrt()),
                    Primitive::BigInt(i128) => Primitive::Float(f64::from(*i128 as i32).sqrt()),
                    Primitive::Byte(u8) => Primitive::Float((*u8 as f64).sqrt()),
                    Primitive::Float(f64) => Primitive::Float(f64.sqrt()),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericToInt => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Int(*i32),
                    Primitive::BigInt(i128) => Primitive::Int(
                        i32::try_from(*i128)
                            .with_context(|| format!("`{i128}` cannot be made into a int"))?,
                    ),
                    Primitive::Byte(u8) => Primitive::Int(*u8 as i32),
                    Primitive::Float(f64) => Primitive::Int(
                        (*f64 as i64)
                            .try_into()
                            .with_context(|| format!("`{f64}` cannot be made into a int"))?,
                    ),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericToBigint => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::BigInt(*i32 as i128),
                    Primitive::BigInt(i128) => Primitive::BigInt(*i128),
                    Primitive::Byte(u8) => Primitive::BigInt(*u8 as i128),
                    Primitive::Float(f64) => Primitive::BigInt((*f64 as i64).into()),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericToByte => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Byte(
                        u8::try_from(*i32)
                            .with_context(|| format!("`{i32}` cannot be made into a byte"))?,
                    ),
                    Primitive::BigInt(i128) => Primitive::Byte(
                        u8::try_from(*i128)
                            .with_context(|| format!("`{i128}` cannot be made into a byte"))?,
                    ),
                    Primitive::Byte(u8) => Primitive::Byte(*u8),
                    Primitive::Float(f64) => Primitive::Byte(
                        (*f64 as i64)
                            .try_into()
                            .with_context(|| format!("`{f64}` cannot be made into a byte"))?,
                    ),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericToFloat => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Float((*i32).into()),
                    Primitive::BigInt(i128) => Primitive::Float(f64::from(
                        i32::try_from(*i128)
                            .with_context(|| format!("`{i128}` cannot be made into a float"))?,
                    )),
                    Primitive::Byte(u8) => Primitive::Float(*u8 as f64),
                    Primitive::Float(f64) => Primitive::Float(*f64),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::GenericAbs => {
                let Some(this) = arguments.first() else {
                    unreachable!()
                };

                let result: Primitive = match this {
                    Primitive::Int(i32) => Primitive::Int(i32.abs()),
                    Primitive::BigInt(i128) => Primitive::BigInt(i128.abs()),
                    Primitive::Byte(u8) => Primitive::Byte(*u8),
                    Primitive::Float(f64) => Primitive::Float(f64.abs()),
                    bad => unreachable!("{bad}"),
                };

                Ok((Some(result), None))
            }
            Self::ByteToAscii => {
                let Some(Primitive::Byte(byte)) = arguments.first() else {
                    unreachable!()
                };

                Ok((
                    Some(Primitive::Str(
                        String::from_utf8_lossy(&[*byte]).into_owned(),
                    )),
                    None,
                ))
            }
            Self::FloatFPart => {
                let Some(Primitive::Float(float)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Float(float.fract())), None))
            }
            Self::FloatIPart => {
                let Some(Primitive::Float(float)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Float(float.trunc())), None))
            }
            Self::FloatRound => {
                let Some(Primitive::Float(float)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Float(float.round())), None))
            }
            Self::FloatFloor => {
                let Some(Primitive::Float(float)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Float(float.floor())), None))
            }
            Self::FloatCeil => {
                let Some(Primitive::Float(float)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Float(float.ceil())), None))
            }
            Self::MapLen => {
                let Some(Primitive::Map(map)) = arguments.first() else {
                    unreachable!()
                };

                Ok((
                    Some(Primitive::Int(map.len().try_into().expect(
                        "map length could not be stored in a 32 bit integer",
                    ))),
                    None,
                ))
            }
            Self::MapHasKey => {
                let (Some(Primitive::Map(map)), Some(key)) = (arguments.first(), arguments.get(1))
                else {
                    unreachable!()
                };

                Ok((Some(Primitive::Bool(map.contains_key(key))), None))
            }
            Self::MapReplace => {
                let (Some(Primitive::Map(map)), Some(key), Some(value)) =
                    (arguments.first(), arguments.get(1), arguments.get(2))
                else {
                    unreachable!()
                };

                let maybe_existing_value = map.insert(key.clone(), value.clone())?;
                Ok((
                    Some(Primitive::Optional(maybe_existing_value.map(Box::new))),
                    None,
                ))
            }
            Self::MapKeys => {
                let Some(Primitive::Map(map)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Vector(GcVector::new(map.keys()))), None))
            }
            Self::MapValues => {
                let Some(Primitive::Map(map)) = arguments.first() else {
                    unreachable!()
                };

                Ok((Some(Primitive::Vector(GcVector::new(map.values()))), None))
            }
            Self::MapPairs => {
                let Some(Primitive::Map(map)) = arguments.first() else {
                    unreachable!()
                };

                Ok((
                    Some(Primitive::Vector(GcVector::new(
                        map.pairs()
                            .into_iter()
                            .map(|(key, value)| Primitive::Vector(GcVector::new(vec![key, value])))
                            .collect::<Vec<_>>(),
                    ))),
                    None,
                ))
            }
        }
    }
}

/// A callback/closure whose variables are mapped at runtime.
/// This struct has no direct _functionality_; it is only meant
/// to store relevant data that can be operated upon.
///
/// While it may seem intuitive, it should be noted that there is
/// no way to call a callback from an FFI interface. Once the interpreter
/// moves to FFI-land, all MScript code will wait until the blocking
/// library finishes its execution.
#[derive(Debug, Clone, Trace, Finalize)]
pub struct PrimitiveFunction {
    /// Used to keep track of where to jump when this function gets called.
    // #[unsafe_ignore_trace]
    location: String,
    /// Each callback has an `Rc` to a [`VariableMapping`]. In other words, a shared
    /// reference to a map of shared references to variables. This allows multiple instances
    /// of the same callback to operate on the same data in a thread-safe manner,
    /// but there is a notable hit to performance.
    callback_state: Option<VariableMapping>,
}

impl Hash for PrimitiveFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.location.hash(state)
    }
}

impl PrimitiveFunction {
    /// Initialize a [`PrimitiveFunction`] given its fields.
    pub(crate) const fn new(path: String, callback_state: Option<VariableMapping>) -> Self {
        Self {
            location: path,
            callback_state,
        }
    }

    /// Initialize a [`PrimitiveFunction`] given its fields. This function will
    /// validate that the path to the function exists before its creation.
    ///
    /// # Errors
    /// This function will propagate I/O errors from traversing symlinks in seRch of
    /// the file. It will also error if the file does not exist.
    pub(crate) fn try_new(path: String, callback_state: Option<VariableMapping>) -> Result<Self> {
        // get the file system path from an MScript function path.
        // ie. path/to/file.mmm#__fn0
        //     ^^^^^^^^^^^^^^^^
        let path_no_function = path.trim_end_matches(|c| c != '#');

        if !Path::try_exists(Path::new(&path_no_function[..path_no_function.len() - 1]))? {
            bail!("path does not exist ({path_no_function})")
        }

        Ok(Self {
            location: path,
            callback_state,
        })
    }

    /// Get the location of the function.
    pub(crate) fn location(&self) -> &str {
        &self.location
    }

    /// Get the variables mapped to this closure.
    pub(crate) fn callback_state(&self) -> &Option<VariableMapping> {
        &self.callback_state
    }

    pub(crate) fn true_eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl PartialEq for PrimitiveFunction {
    /// Avoid comparing the variable mapping, which should always be the same
    /// in the case where [`PrimitiveFunction::location`]'s are equal.
    fn eq(&self, other: &Self) -> bool {
        self.location == other.location
    }
}

impl Display for PrimitiveFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function ptr {}()", self.location)?;

        if let Some(ref callback_state) = self.callback_state {
            write!(f, " + pool@+{}", callback_state.len())?;
        };

        Ok(())
    }
}

/// This enum represents the return state of a function.
/// As of now, the only valid states are:
/// * `FFIError` (produced **exclusively** by the `raise_error!` macro)
/// * `NoValue`
/// * `Value`
#[derive(Debug, Clone)]
pub enum ReturnValue {
    /// This variant is produced when calling an FFI function produces an expected error,
    /// as opposed to directly panicking. While rust FFI `panic!`'s should be caught by a
    /// [`std::panic::catch_unwind`], other exceptions and branches in control flow
    /// (like C++ exceptions, `longjmp`, `setjmp`) will produce undefined behavior.
    FFIError(String),
    /// Produced by functions that return `()`/`void`
    NoValue,
    /// This variant is produced when a function exits succesfully and produces a value.
    Value(Primitive),
}

impl ReturnValue {
    /// Get the return value of this function, but only if one exists.
    pub fn get(self) -> Option<Primitive> {
        if let ReturnValue::Value(primitive) = self {
            Some(primitive)
        } else {
            None
        }
    }
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FFIError(message) => write!(f, "{message}"),
            Self::Value(primitive) => write!(f, "{primitive}"),
            Self::NoValue => write!(f, "None"),
        }
    }
}

/// This enum represents the valid return states of a **single** bytecode instruction.
///
/// All [`InstructionExitState`] variants can be sent up from an [`Instruction`] to its
/// parent [`Function`] via the [`Ctx`] bridge.
///
/// When doing so, the flag sent can be viewed as a request for the interpreter to:
/// * execute code
/// * change its behavior
/// * jump around
///
/// A key word is **request**. The interpreter does not have to heed every request,
/// but most requests are infallible and guaranteed to be processed.
#[derive(Debug)]
pub enum InstructionExitState {
    /// This variant serves to signal that an instruction has a [`ReturnValue`] it would
    /// like the function to deal with.
    ReturnValue(ReturnValue),
    /// This variant serves to signal that an instruction has a [`JumpRequest`] it would
    /// like the function to deal with.
    JumpRequest(JumpRequest),
    /// This variant signals that an instruction would like the interpreter to jump forward
    /// `+isize` instructions.
    ///
    /// # Errors
    /// This request will fail if the requested interpreter cursor
    /// position would fall out of bounds.
    Goto(isize),
    /// This variant requests to push a [`SpecialScope`] to the interpreter process. This scope
    /// can be used for identification.
    PushScope(SpecialScope),
    /// This variant requests to:
    /// * push a [`SpecialScope`] to the interpreter process
    /// * jump forward `+usize` instructions.
    ///
    /// # Errors
    /// This request will fail if the requested interpreter cursor
    /// position would fall out of bounds.
    GotoPushScope(usize, SpecialScope),
    /// This variant requests to:
    /// * pop a [`SpecialScope`] from the interpreter process
    /// * jump forward `+usize` instructions.
    GotoPopScope(isize, usize),
    /// This variant requests to pop a [`SpecialScope`] from the interpreter process.
    ///
    /// # Errors
    /// This request will fail if the interpreter does not have any special scopes on its stack.
    PopScope,
    /// # Default
    ///
    /// This is the standard exit state. The interpreter will move on to the next instruction
    /// if it encounters this variant.
    NoExit,
    BeginNotificationBridge(Box<dyn RuntimeExecutionBridgeNotifier>),
}

pub trait RuntimeExecutionBridgeNotifier: Debug {
    fn wait_for(&self) -> Result<JumpRequest>;
    fn then(&self, return_value: ReturnValue) -> Result<bool>;
    fn finish(&self) -> Result<Option<Primitive>>;
}

/// This is MScript's main unit of executing instructions.
///
/// Along with a function's name and location, this struct
/// packs together all of the bytecode instructions needed to run
/// the code.
///
/// In essense, this struct exposes an interface that allows users to
/// run raw bytecode. (See `Function::run()`)
pub struct Function {
    /// A shared reference to the file of origin.
    location: Weak<MScriptFile>,
    /// A list of the instructions this subroutine consists of.
    instructions: Box<[Instruction]>,
    /// The name of this function.
    name: String,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.get_qualified_name();
        write!(f, "{name} - {} instructions total", self.instructions.len())
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} function {}",
            self.location.upgrade().unwrap().path(),
            self.name
        )
    }
}

impl Function {
    /// Initialize a [`Function`] given its fields.
    pub(crate) const fn new(
        location: Weak<MScriptFile>,
        name: String,
        instructions: Box<[Instruction]>,
    ) -> Self {
        Self {
            location,
            name,
            instructions,
        }
    }

    /// Get the "representation" of a [`Function`]. In other words,
    /// combine a [`Function`]'s path with its name to create an MScript Function Path.
    ///
    /// # Examples
    /// ```ignore
    /// use std::sync::Rc;
    ///
    /// let file = MScriptFile::open("path/to/file.mmm")?;
    /// let instructions = ...;
    /// let function = Function::new(Rc::new(file), "add_numbers".into(), instructions);
    ///
    /// assert_eq!(function.get_qualified_name(), "path/to/file.mmm#add_numbers")
    /// ```
    #[inline]
    pub(crate) fn get_qualified_name(&self) -> String {
        format!("{}#{}", self.location.upgrade().unwrap().path(), self.name)
    }

    /// This is the backbone of the interpreter. This function creates the main event
    /// loop for each function, and handles jumps. Because of this, it is **very complex**
    /// and has many ways to diverge.
    ///
    /// # Arguments
    /// * `args` - The arguments to this function, provided by the caller.
    /// * `current_frame` - A shared reference to the current stack trace. The caller
    ///                     **SHOULD NOT** push a stack frame for a bytecode function
    ///                     before calling it; this method will handle that.
    /// * `callback_state` - A shared reference to the [`VariableMapping`] that this
    ///                      function can access. This argument is used for callbacks
    ///                      and closures exclusively. Normal variables should be
    ///                      added to `current_frame`.
    /// * `jump_callback` - A callback that defines how this function's jumps are handled.
    ///                     The implementation is up to the caller.
    ///
    /// # Errors
    /// This function can error if an instruction raises an error during execution.
    ///
    /// # Panics
    /// This function will `panic!` if the instruction byte falls outside of (0..[`INSTRUCTION_COUNT`][crate::instruction_constants::INSTRUCTION_COUNT])
    pub(crate) fn run(
        &self,
        args: Cow<Vec<Primitive>>,
        current_frame: Rc<RefCell<Stack>>,
        callback_state: Option<VariableMapping>,
        jump_callback: &mut impl Fn(&JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        {
            current_frame
                .borrow_mut()
                .extend(Cow::Owned(self.get_qualified_name()));
        }

        // Each function needs its own context.
        let mut context = Ctx::new(self, current_frame.clone(), args, callback_state);

        // The index into the instruction array.
        let mut instruction_ptr = 0;

        let mut special_scopes: Vec<SpecialScope> = vec![];

        let instruction_len = self.instructions.len();

        while instruction_ptr < self.instructions.len() {
            let instruction = &self.instructions[instruction_ptr];

            // queries the function pointer associated with the instruction,
            // and gives it ownership of the instruction.
            query!(&mut context, instruction)
                .context("failed to run instruction")
                .with_context(|| {
                    let instruction_as_str =
                        raw_byte_instruction_to_string_representation(instruction.id)
                            .unwrap_or(Cow::Borrowed("Unknown Instruction"));

                    format!(
                        "{instruction_as_str:?} (instruction #{instruction_ptr} of {})",
                        self.name
                    )
                })?;

            // `context` must have its exit state cleared before continuing the loop.
            let ret: &InstructionExitState = context.poll();

            let old_ptr_location = instruction_ptr;

            let mut goto_fn = |offset: isize| -> Result<()> {
                #[cfg(feature = "debug")]
                let new_val = instruction_ptr
                    .checked_add_signed(offset)
                    .with_context(|| format!("numeric overflow ({instruction_ptr} + {offset})"))?;
                #[cfg(not(feature = "debug"))]
                let new_val = (instruction_ptr as isize + offset) as usize;

                if new_val >= instruction_len {
                    bail!("goto position index {new_val} is too big, instruction length is {instruction_len}.");
                }

                instruction_ptr = new_val;
                Ok(())
            };

            // process the exit state
            match ret {
                InstructionExitState::ReturnValue(ret) => {
                    current_frame.borrow_mut().pop_until_function();
                    return Ok(ret.clone());
                }
                InstructionExitState::JumpRequest(jump_request) => {
                    let result = jump_callback(jump_request)?;

                    if let ReturnValue::FFIError(message) = result {
                        bail!("FFI: {message}")
                    }

                    if let Some(primitive) = result.get() {
                        context.push(primitive)
                    }
                }
                InstructionExitState::Goto(offset) => {
                    goto_fn(*offset)?;
                    context.clear_signal();
                    continue;
                }
                InstructionExitState::PushScope(ty) => {
                    special_scopes.push(*ty);
                    context.add_frame(Cow::Borrowed(ty.identity_str()));
                }
                InstructionExitState::GotoPushScope(offset, ty) => {
                    goto_fn((*offset).try_into()?)?;

                    special_scopes.push(*ty);
                    context.add_frame(Cow::Borrowed(ty.identity_str()));

                    context.clear_signal();
                    continue;
                }
                ref x @ InstructionExitState::GotoPopScope(offset, frames_to_pop) => {
                    goto_fn(*offset)?;

                    for _ in 0..*frames_to_pop {
                        log::trace!("{x:?} at i#{old_ptr_location} of {}, new location is {instruction_ptr} ({:?})", self.get_qualified_name(), raw_byte_instruction_to_string_representation(self.instructions[instruction_ptr].id));

                        context.pop_frame();
                    }

                    context.clear_signal();
                    continue;
                }
                InstructionExitState::PopScope => {
                    if let Some(x) = special_scopes.pop() {
                        log::trace!("PopScope `{x:?}` at {instruction_ptr}");
                        context.pop_frame();
                    }
                }
                InstructionExitState::BeginNotificationBridge(bridge) => {
                    loop {
                        let to_call = bridge.wait_for()?;
                        let return_value = jump_callback(&to_call)?;
                        if !bridge.then(return_value)? {
                            break;
                        }
                    }
                    if let Some(final_exit_state) = bridge.finish()? {
                        context.push(final_exit_state);
                    }
                }
                InstructionExitState::NoExit => (),
            }

            context.clear_signal();

            instruction_ptr += 1;
        }

        // Handle when a function does not explicitly return.
        log::warn!("Warning: function concludes without `ret` instruction");

        current_frame.borrow_mut().pop();

        Ok(ReturnValue::NoValue)
    }

    /// Get a function's name.
    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    /// Get a function's location.
    pub(crate) fn location(&self) -> Weak<MScriptFile> {
        Weak::clone(&self.location)
    }
}

/// A map of a function's name to the function struct.
#[derive(Debug)]
pub struct Functions {
    pub(crate) map: HashMap<String, Function>,
}

impl<'a> Functions {
    /// Initialize a [`Functions`] map given its fields.
    pub(crate) const fn new(map: HashMap<String, Function>) -> Self {
        Self { map }
    }

    pub(crate) fn new_empty() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub(crate) fn run_function(
        &self,
        name: &String,
        args: Cow<Vec<Primitive>>,
        current_frame: Rc<RefCell<Stack>>,
        callback_state: Option<VariableMapping>,
        jump_callback: &mut impl Fn(&JumpRequest) -> Result<ReturnValue>,
    ) -> Result<ReturnValue> {
        let Some(function) = &self.map.get(name) else {
            panic!("not found: {name} in {self:?}");
        };

        function.run(args, current_frame, callback_state, jump_callback)
    }

    pub(crate) fn add_function(
        &mut self,
        file: Weak<MScriptFile>,
        name: String,
        bytecode: Box<[Instruction]>,
    ) -> Option<Function> {
        let function = Function::new(file, name.clone(), bytecode);

        // TODO: If Rc<name> == self.map.name, we can change Function::name to be the same Rc.

        self.map.insert(name, function)
    }

    /// Get a reference to a function with name `signature`, if it exists.
    pub(crate) fn get(&self, signature: &String) -> Result<&Function> {
        let result = self
            .map
            .get(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }

    /// Get a mutable reference to a function with name `signature`, if it exists.
    pub(crate) fn get_mut(&'a mut self, signature: &String) -> Result<&'a mut Function> {
        let result = self
            .map
            .get_mut(signature)
            .with_context(|| format!("unknown function ({signature})"))?;
        Ok(result)
    }
}
