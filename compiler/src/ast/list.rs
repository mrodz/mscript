use std::{borrow::Cow, fmt::Display};

use anyhow::{Result, bail, Context};

use crate::{
    ast::{CompileTimeEvaluate, TemporaryRegister, new_err},
    instruction,
    parser::{Node, Parser}, VecErr,
};

use super::{
    r#type::IntoType,
    ConstexprEvaluation,
    Compile, Dependencies, TypeLayout, Value, map_err, value::ValToUsize
};

#[derive(Debug)]
pub(crate) struct List {
    values: Vec<Value>,
}

impl List {
    pub fn for_type_force_mixed(&self) -> Result<TypeLayout> {
        let mut types = Vec::with_capacity(self.values.len());
        for value in &self.values {
            let value_ty = value.for_type()?;
            types.push(Cow::Owned(value_ty));
        }

        Ok(TypeLayout::List(ListType::Mixed(types)))
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

impl CompileTimeEvaluate for List {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        let mut result = Vec::with_capacity(self.values.len());

        for value in &self.values {
            let constexpr_eval = value.try_constexpr_eval()?;
            let Some(value) = constexpr_eval.into_owned() else {
                return Ok(ConstexprEvaluation::Impossible);
            };

            result.push(value);
        }

        Ok(ConstexprEvaluation::Owned(Value::List(List { values: result })))
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ListBound {
    Numeric(usize),
    NotIndexable,
    Infinite,
}

impl ListBound {
    pub(crate) fn val_fits_between(start: &Self, end: &Self, value: &Value) -> Result<bool> {
        if matches!(start, Self::NotIndexable) {
            bail!("this list is not indexable (is it empty?)")
        }

        match end {
            Self::Infinite => Ok(true),
            Self::Numeric(last_valid_index) => {
                let ConstexprEvaluation::Owned(value) = value.try_constexpr_eval()? else {
                    bail!("Cannot guarantee that this operation will not fail, as it is a non-constexpr index.\nTo allow fallable lookups, explicitly give a spread type to the list:\n```\n\tvar: [int...] = [1, 2, 3]\n```")
                };

                let ValToUsize::Ok(value) = value.get_usize().context("cannot index with this type")? else {
                    bail!("error converting to numeric index")
                };

                if value > *last_valid_index {
                    bail!("operation will be out of bounds; cannot index with `{value}` into list whose known valid safe indexes are 0..{last_valid_index}")
                }

                Ok(true)
            }
            _ => unreachable!()
        }
    }
}

impl Display for ListBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Numeric(index) => write!(f, "{index}"),
            Self::NotIndexable => write!(f, "!"),
            Self::Infinite => write!(f, "∞"),
        }
    }
}

impl ListType {
    pub fn must_be_const(&self) -> bool {
        if let Self::Open { types, .. } = self {
            if !types.is_empty() {
                return true;
            }
        }

        false
    }

    // pub fn get_type_at_index(&self, index: ) {
    //     let maybe_constexpr_eval = self.try_constexpr_eval()?;
    //     let Some(constexpr_value) = maybe_constexpr_eval.as_ref() else {
    //         bail!("an index that is not compile-time-constant cannot be used to deduce the type of the resulting element");
    //     };

    //     let Value::Number(number) = constexpr_value else {
    //         unreachable!();
    //     };

    //     match number {
    //         Number::BigInt(idx) | Number::Integer(idx) | Number::Byte(idx) => {
    //             let idx: usize = idx.parse()?;

    //             let Some(type_at_index) = list_type.get_type_at_index(idx) else {
    //                 bail!("`{idx}` is not a valid index into `{list_type}`, which has a known length at compile time of {}", list_type.len())
    //             };
    //             Ok(Cow::Owned(type_at_index.clone()))
    //         }
    //         Number::Float(_) => bail!("cannot index into a list with floats"),
    //     }
    // }

    pub fn lower_bound(&self) -> ListBound {
        match self {
            Self::Empty => ListBound::NotIndexable,
            _ => ListBound::Numeric(0)
        }
    }

    pub fn upper_bound(&self) -> ListBound {
        match self {
            Self::Empty => ListBound::NotIndexable,
            Self::Open { .. } => ListBound::Infinite,
            Self::Mixed(types) => ListBound::Numeric(types.len() - 1)
        }
    }

    pub fn valid_indexes(&self) -> (ListBound, ListBound) {
        (self.lower_bound(), self.upper_bound())
    }

    #[allow(unused)]
    pub fn len(&self) -> Cow<'static, str> {
        match self {
            Self::Empty => Cow::Borrowed("0"),
            Self::Open { .. } => Cow::Borrowed("∞"),
            Self::Mixed(types) => Cow::Owned(types.len().to_string()),
        }
    }

    pub fn get_type_at_known_index(&self, index: usize) -> Result<&TypeLayout> {
        match self {
            Self::Empty => bail!("empty list"),
            Self::Mixed(types) => types.get(index).map(Cow::as_ref).context("there is no value at the specified index"),
            Self::Open { types, spread } => Ok(if let Some(ty) = types.get(index) {
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

                    lhs_all_the_same && rhs_all_the_same
                }
                Mixed(t2) => {
                    for (idx, ty2) in t2.iter().enumerate() {
                        let ty1 = types.get(idx).unwrap_or(spread);

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

impl IntoType for Index {
    fn for_type(&self) -> Result<TypeLayout> {
        Ok(self.final_output_type.clone())
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
    parts: Box<[Value]>,
    final_output_type: TypeLayout,
}

impl Dependencies for Index {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.parts.iter().flat_map(Value::net_dependencies).collect()
    }
}

impl CompileTimeEvaluate for Index {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        if self.parts.len() == 1 {
            self.parts[0].try_constexpr_eval()
        } else {
            Ok(ConstexprEvaluation::Impossible)
        }
        // if self.0.is_empty() {
        //     unreachable!("empty index");
        // }

        // let mut values = vec![];

        // for index in self.0.iter() {
        //     let index = index.try_constexpr_eval()?;
        //     let ConstexprEvaluation::Owned(value) = index else {
        //         return Ok(ConstexprEvaluation::Impossible);
        //     };

        //     values.push(value);
        // }

        // Ok(ConstexprEvaluation::Owned(Value::MultiIndex(values.into_boxed_slice())))
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

        let registerc = self.parts.len();

        let mut result = vec![];
        for i in 0..registerc {
            let lhs_register = TemporaryRegister::new(); // TemporaryRegister::reserve_many(registerc);
            result.push(instruction!(store_fast lhs_register));
            let index_temp_register = TemporaryRegister::new();

            let mut val_init = self.parts[i].compile(function_buffer)?;
            result.append(&mut val_init);

            let instruction_str = format!("[{index_temp_register}]");


            result.append(&mut vec![
                instruction!(store_fast index_temp_register),
                instruction!(delete_name_reference_scoped lhs_register),
                instruction!(vec_op instruction_str),
                instruction!(delete_name_scoped index_temp_register)
            ]);

            index_temp_register.free();
            lhs_register.free();
        }

        // un-comment

    //     result.append(&mut self.0.compile(function_buffer)?);

    //     let index_temp_register = TemporaryRegister::new();

    //     let instruction_str = format!("[{index_temp_register}]");

    //    ;

    //     index_temp_register.free();
    //     vec_temp_register.free();

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

    pub fn list_index(input: Node, starting_type: TypeLayout) -> Result<Index, Vec<anyhow::Error>> {
        let children = input.children();

        let file_name: &str = &input.user_data().get_source_file_name();

        let mut last_ty = starting_type;

        let mut values = vec![];

        for value in children {
            let value_span = value.as_span();
            let value = Self::value(value)?;
            let value_ty = value.for_type().to_err_vec()?;
            
            let Some(expected_type_for_value) = last_ty.supports_index() else {
                return Err(vec![new_err(value_span, file_name, format!("`{last_ty}` does not support indexing, yet here `{value_ty}` is used"))]);
            };

            if !expected_type_for_value.contains(&value, value_span, file_name).to_err_vec()? {
                return Err(vec![new_err(value_span, file_name, format!("`{last_ty}` does not support indexing for `{value_ty}` (Hint: this type does support indexes of types {expected_type_for_value})"))]);
            }

            let index_output_ty = map_err(last_ty.get_output_type_from_index(&value), value_span, file_name, format!("invalid index into `{last_ty}`")).to_err_vec()?;
            
            last_ty = index_output_ty.into_owned();
            values.push(value);
        }

        Ok(Index {
            parts: values.into_boxed_slice(),
            final_output_type: last_ty
        })
    }
}
