use std::{borrow::Cow, fmt::Display};

use anyhow::{bail, Result};

use crate::{
    ast::{value::CompileTimeEvaluate, TemporaryRegister, STR_TYPE},
    instruction,
    parser::{Node, Parser},
};

use super::{
    r#type::{IntoType, NativeType},
    value::{ConstexprEvaluation, Indexable, ValueChain},
    Compile, Dependencies, Number, TypeLayout, Value,
};

#[derive(Debug)]
pub(crate) struct List {
    values: Vec<ValueChain>,
}

impl List {
    #[allow(unused)]
    pub fn type_at_index(&self, index: usize) -> Result<Cow<'static, TypeLayout>> {
        if let Some(value) = self.values.get(index) {
            return Ok(Cow::Owned(value.for_type()?));
        }

        bail!("out of bounds")
    }

    pub fn for_type_force_mixed(&self) -> Result<TypeLayout> {
        let mut types = Vec::with_capacity(self.values.len());
        for value in &self.values {
            let value_ty = value.for_type()?;
            types.push(Cow::Owned(value_ty));
        }

        Ok(TypeLayout::List(ListType::Mixed(types)))
    }

    pub fn get_value(&self, index: usize) -> Option<&ValueChain> {
        self.values.get(index)
    }
}

impl Compile for List {
    fn compile(
        &self,
        function_buffer: &mut Vec<super::CompiledItem>,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let initial_capacity = self.values.len();

        let vec_init_register = TemporaryRegister::new();

        let vec_op_str = "+".to_owned() + &vec_init_register.to_string();

        let mut result = vec![
            instruction!(make_vector initial_capacity),
            instruction!(store_fast vec_init_register),
        ];

        for value in &self.values {
            let mut value_init = value.compile(function_buffer)?;
            result.append(&mut value_init);
            result.push(instruction!(vec_op vec_op_str));
        }

        result.push(instruction!(delete_name_reference_scoped vec_init_register));

        vec_init_register.free();

        Ok(result)
    }
}

impl Dependencies for List {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.values
            .iter()
            .flat_map(|x| x.net_dependencies())
            .collect()
    }
}

#[derive(Debug, Clone, Eq)]
pub(crate) enum ListType {
    Mixed(Vec<Cow<'static, TypeLayout>>),
    Open {
        types: Vec<Cow<'static, TypeLayout>>,
        spread: Box<Cow<'static, TypeLayout>>,
    },
    Empty,
}

impl ListType {
    pub fn must_be_const(&self) -> bool {
        if let Self::Open { types, .. } = self {
            if !types.is_empty() {
                return true;
            }
        }

        return false;
    }

    pub fn len(&self) -> Cow<'static, str> {
        match self {
            Self::Empty => Cow::Borrowed("0"),
            Self::Open { .. } => Cow::Borrowed("∞"),
            Self::Mixed(types) => Cow::Owned(types.len().to_string()),
        }
    }

    pub fn get_type_at_index(&self, index: usize) -> Option<&TypeLayout> {
        match self {
            Self::Empty => None,
            Self::Mixed(types) => types.get(index).map(Cow::as_ref),
            Self::Open { types, spread } => Some(if let Some(ty) = types.get(index) {
                ty.as_ref()
            } else {
                spread.as_ref()
            }),
        }
    }
}

impl Display for ListType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "[]"),
            Self::Mixed(types) => {
                let first = types.first().unwrap();

                write!(f, "[{first}")?;

                for ty in &types[1..] {
                    write!(f, ", {ty}")?;
                }

                write!(f, "]")
            }
            Self::Open { types, spread } => {
                write!(f, "[")?;
                if !types.is_empty() {
                    for ty in types {
                        write!(f, "{ty}, ")?;
                    }
                }

                write!(f, "{spread}...]")
            }
        }
    }
}

impl PartialEq for ListType {
    fn eq(&self, other: &Self) -> bool {
        use ListType::*;

        match (self, other) {
            (Empty, Empty) => true,
            (Empty, _) | (_, Empty) => false,
            (Mixed(t1), Mixed(t2)) => t1 == t2,
            (Open { types, spread }, other) | (other, Open { types, spread }) => match other {
                Open {
                    types: t2,
                    spread: s2,
                } => {
                    if types == t2 && spread == s2 {
                        return true;
                    }

                    let mut lhs_all_the_same = true;

                    let lhs_ty = spread;

                    for ty in types {
                        if ty != lhs_ty.as_ref() {
                            lhs_all_the_same = false;
                        }
                    }

                    let mut rhs_all_the_same = true;

                    let rhs_ty = s2;

                    for ty in t2 {
                        if ty != rhs_ty.as_ref() {
                            rhs_all_the_same = false;
                        }
                    }

                    return lhs_all_the_same && rhs_all_the_same;
                }
                Mixed(t2) => {
                    for (idx, ty2) in t2.iter().enumerate() {
                        let ty1 = types.get(idx).unwrap_or_else(|| spread);

                        if ty1 != ty2 {
                            return false;
                        }
                    }

                    true
                }
                _ => unreachable!(),
            },
        }
    }
}

impl IntoType for List {
    fn for_type(&self) -> Result<TypeLayout> {
        if self.values.is_empty() {
            Ok(TypeLayout::List(ListType::Empty))
        } else {
            let size = self.values.len() - 1;

            let mut last_ty: Option<TypeLayout> = None;

            let mut all_are_the_same_type = true;

            let mut end_of_unique_values = size;
            for (idx, value) in self.values.iter().enumerate().rev() {
                let this_ty = value.for_type()?.get_owned_type_recursively();

                if let Some(ref mut last_ty) = last_ty {
                    if last_ty.get_type_recursively() != &this_ty && end_of_unique_values == size {
                        end_of_unique_values = idx;
                        all_are_the_same_type = false;
                        // break 'label idx;
                    }
                } else {
                    last_ty = Some(this_ty);
                }
            }

            if end_of_unique_values != size {
                let mut ty_mixed_list = Vec::with_capacity(size + 1);

                for value in &self.values {
                    ty_mixed_list.push(Cow::Owned(value.for_type()?));
                }

                return Ok(TypeLayout::List(ListType::Mixed(ty_mixed_list)));
            }

            let last_ty: TypeLayout = self.values.last().unwrap().for_type()?;

            if all_are_the_same_type {
                return Ok(TypeLayout::List(ListType::Open {
                    types: vec![],
                    spread: Box::new(Cow::Owned(last_ty)),
                }));
            }

            let mut types: Vec<Cow<'static, TypeLayout>> = Vec::with_capacity(size);

            for value in &self.values[..end_of_unique_values] {
                types.push(Cow::Owned(value.for_type()?));
            }

            Ok(TypeLayout::List(ListType::Open {
                types,
                spread: Box::new(Cow::Owned(last_ty)),
            }))
        }
    }
}

#[derive(Debug)]
pub(crate) struct Index {
    value: ValueChain,
}

impl Dependencies for Index {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.value.net_dependencies()
    }
}

impl CompileTimeEvaluate for Index {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        self.value.try_constexpr_eval()
    }
}

impl Indexable for Index {
    fn output_from_value(&self, value: &Value) -> Result<Cow<'static, TypeLayout>> {
        if !self.value.for_type()?.is_numeric_index() {
            bail!("for now, indexing can only be done with integers")
        }

        let value_ty = value.for_type()?;

        match value_ty {
            TypeLayout::Native(NativeType::Str) => Ok(Cow::Borrowed(*STR_TYPE)),
            TypeLayout::List(list_type) => {
                let maybe_constexpr_eval = self.value.try_constexpr_eval()?;
                let Some(constexpr_value) = maybe_constexpr_eval.as_ref() else {
                    bail!("an index that is not compile-time-constant cannot be used to deduce the type of the resulting element");
                };

                let Value::Number(number) = constexpr_value else {
                    unreachable!();
                };

                match number {
                    Number::BigInt(idx) | Number::Integer(idx) | Number::Byte(idx) => {
                        let idx: usize = idx.parse()?;

                        let Some(type_at_index) = list_type.get_type_at_index(idx) else {
                            bail!("`{idx}` is not a valid index into `{list_type}`, which has a known length at compile time of {}", list_type.len())
                        };

                        Ok(Cow::Owned(type_at_index.clone()))
                    }
                    Number::Float(_) => bail!("cannot index into a list with floats"),
                }
            }

            other => unimplemented!("{other:?}"),
        }
    }
}

impl Compile for Index {
    fn compile(
        &self,
        function_buffer: &mut Vec<super::CompiledItem>,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let value = self.try_constexpr_eval()?;

        // faster lookup if the value is already known.
        if let Some(Value::Number(number)) = value.as_ref() {
            let index: usize = number.try_into()?;
            let instruction_str = format!("[{index}]");
            return Ok(vec![instruction!(vec_op instruction_str)]);
        }

        let vec_temp_register = TemporaryRegister::new();

        let mut result = vec![instruction!(store_fast vec_temp_register)];

        result.append(&mut self.value.compile(function_buffer)?);

        let index_temp_register = TemporaryRegister::new();

        let instruction_str = format!("[{index_temp_register}]");

        result.append(&mut vec![
            instruction!(store_fast index_temp_register),
            instruction!(load_fast vec_temp_register),
            instruction!(vec_op instruction_str),
        ]);

        index_temp_register.free();
        vec_temp_register.free();

        Ok(result)
    }
}

impl Parser {
    pub fn list(input: Node) -> Result<List, Vec<anyhow::Error>> {
        let mut values = vec![];

        for value_node in input.children() {
            values.push(Self::value(value_node)?);
        }

        let list = List { values };

        Ok(list)
    }

    pub fn list_index(input: Node) -> Result<Index, Vec<anyhow::Error>> {
        let value_node = input.children().single().unwrap();
        let value = Self::value(value_node)?;

        Ok(Index { value })
    }
}
