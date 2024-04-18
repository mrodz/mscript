use std::{borrow::Cow, fmt::Display, hash::Hash};

use anyhow::{bail, Context, Result};

use crate::{
    ast::{new_err, r#type::TypecheckFlags, ClassType, CompileTimeEvaluate},
    instruction,
    parser::{Node, Parser},
    VecErr,
};

use super::{
    map_err, r#type::IntoType, value::ValToUsize, CompilationState, Compile, ConstexprEvaluation,
    Dependencies, TypeLayout, Value,
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
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let initial_capacity = self.values.len();

        let vec_init_register = state.poll_temporary_register();

        let vec_op_str = "+".to_owned() + &vec_init_register.to_string();

        let mut result = vec![
            instruction!(make_vector initial_capacity),
            instruction!(store_fast vec_init_register),
        ];

        for value in &self.values {
            let mut value_init = value.compile(state)?;
            result.append(&mut value_init);
            result.push(instruction!(vec_op vec_op_str));
        }

        result.push(instruction!(delete_name_reference_scoped vec_init_register));

        state.free_temporary_register(vec_init_register);

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

        Ok(ConstexprEvaluation::Owned(Value::List(List {
            values: result,
        })))
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
    Open(Box<Cow<'static, TypeLayout>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ListBound {
    Numeric(usize),
    Infinite,
}

impl ListBound {
    pub(crate) fn val_fits_between(end: &Self, value: &Value) -> Result<bool> {
        match end {
            Self::Infinite => Ok(true),
            Self::Numeric(last_valid_index) => {
                let ConstexprEvaluation::Owned(value) = value.try_constexpr_eval()? else {
                    bail!("Cannot guarantee that this operation will not fail, as it is a non-constexpr index.\nTo allow fallable lookups, explicitly give a spread type to the list:\n```\n\tvar: [int...] = [1, 2, 3]\n```")
                };

                let ValToUsize::Ok(value) =
                    value.get_usize().context("cannot index with this type")?
                else {
                    bail!("error converting to numeric index")
                };

                if value > *last_valid_index {
                    bail!("operation will be out of bounds; cannot index with `{value}` into list whose known valid safe indexes are 0..{last_valid_index}")
                }

                Ok(true)
            }
        }
    }
}

impl Display for ListBound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Numeric(index) => write!(f, "{index}"),
            Self::Infinite => write!(f, "∞"),
        }
    }
}

impl ListType {
    pub fn must_be_const(&self) -> bool {
        matches!(self, Self::Mixed(..))
    }

    pub fn upper_bound(&self) -> ListBound {
        match self {
            Self::Open { .. } => ListBound::Infinite,
            Self::Mixed(types) => ListBound::Numeric(types.len() - 1),
        }
    }

    pub fn try_coerce_to_open(
        &self,
        comparison_flags: &TypecheckFlags<&ClassType>,
    ) -> Result<Cow<Self>> {
        match self {
            ret @ Self::Open(..) => Ok(Cow::Borrowed(ret)),
            Self::Mixed(types) => {
                if types
                    .iter()
                    .as_ref()
                    .windows(2)
                    .all(|x| x[0].eq_complex(&x[1], comparison_flags))
                {
                    if let Some(ty) = types.first() {
                        Ok(Cow::Owned(ListType::Open(Box::new(ty.clone()))))
                    } else {
                        bail!("cannot know the type of this list, for it is empty")
                    }
                } else {
                    bail!("this is a mixed-type list")
                }
            }
        }
    }

    pub fn valid_indexes(&self) -> ListBound {
        self.upper_bound()
    }

    pub fn get_type_at_known_index(&self, index: usize) -> Result<&TypeLayout> {
        match self {
            Self::Mixed(types) => types
                .get(index)
                .map(Cow::as_ref)
                .context("there is no value at the specified index"),
            Self::Open(ty) => Ok(ty.as_ref()),
        }
    }
}

impl Display for ListType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mixed(types) => {
                write!(f, "[")?;

                if let Some(ty) = types.first() {
                    write!(f, "{ty}")?;
                }

                if let Some(remainder) = types.get(1..) {
                    for ty in remainder {
                        write!(f, ", {ty}")?;
                    }
                }

                write!(f, "]")
            }
            Self::Open(ty) => {
                write!(f, "[{ty}...]")
            }
        }
    }
}

impl PartialEq for ListType {
    fn eq(&self, other: &Self) -> bool {
        use ListType as E;

        let typecheck_flags: TypecheckFlags<&ClassType> = TypecheckFlags::classless();

        match (self, other) {
            (E::Mixed(t1), E::Mixed(t2)) => t1
                .iter()
                .zip(t2.iter())
                .all(|(x, y)| x.eq_complex(y, &typecheck_flags)),
            (E::Open(t1), E::Open(t2)) => t1.eq_complex(t2, &typecheck_flags),
            (E::Mixed(t1), E::Open(t2)) | (E::Open(t2), E::Mixed(t1)) => {
                for ty in t1 {
                    if !t2.eq_complex(ty, &TypecheckFlags::<&ClassType>::classless()) {
                        return false;
                    }
                }
                true
            }
        }
    }
}

impl Hash for ListType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use ListType::*;

        match self {
            Mixed(types) => {
                if types.windows(2).all(|w| w[0] == w[1]) {
                    types[0].hash(state);
                } else {
                    types.hash(state);
                }
            }
            Open(ty) => ty.hash(state),
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
        let mut types = self.values.iter().map(Value::for_type).collect::<Vec<_>>();

        if types.len() == 1 {
            // `swap_remove` to get ownership in O(1)
            return Ok(TypeLayout::List(ListType::Open(Box::new(Cow::Owned(
                types.swap_remove(0)?,
            )))));
        }

        let mut no_result = Vec::with_capacity(types.len());

        for ty in types {
            no_result.push(Cow::Owned(ty?));
        }

        Ok(TypeLayout::List(ListType::Mixed(no_result)))
    }
}

#[derive(Debug)]
pub(crate) struct Index {
    origin_is_map: bool,
    parts: Box<[Value]>,
    final_output_type: TypeLayout,
}

impl Index {
    pub fn final_output_type(&self) -> &TypeLayout {
        &self.final_output_type
    }
}

impl Dependencies for Index {
    fn dependencies(&self) -> Vec<super::Dependency> {
        self.parts
            .iter()
            .flat_map(Value::net_dependencies)
            .collect()
    }
}

impl CompileTimeEvaluate for Index {
    fn try_constexpr_eval(&self) -> Result<ConstexprEvaluation> {
        if self.parts.len() == 1 {
            self.parts[0].try_constexpr_eval()
        } else {
            Ok(ConstexprEvaluation::Impossible)
        }
    }
}

impl Compile for Index {
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        let value = self.try_constexpr_eval()?;

        // faster lookup if the value is already known.
        if let (false, Some(Value::Number(number))) = (self.origin_is_map, value.as_ref()) {
            let index: usize = number.try_into()?;
            let instruction_str = format!("[{index}]");
            return Ok(vec![instruction!(vec_op instruction_str)]);
        }

        let registerc = self.parts.len();

        let mut result = vec![];
        for i in 0..registerc {
            let lhs_register = state.poll_temporary_register();
            result.push(instruction!(store_fast lhs_register));
            let index_temp_register = state.poll_temporary_register();

            let mut val_init = self.parts[i].compile(state)?;
            result.append(&mut val_init);

            if dbg!(self.parts[i].for_type().unwrap()).is_map() {
                result.append(&mut vec![
                    instruction!(map_op index_temp_register)
                ]);
            } else {
                result.append(&mut vec![
                    instruction!(store_fast index_temp_register),
                    instruction!(delete_name_reference_scoped lhs_register),
                    instruction!(vec_op(format!("[{index_temp_register}]"))),
                ]);
            }

            state.free_temporary_register(index_temp_register);
            state.free_temporary_register(lhs_register);
        }

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
        let origin_is_map = starting_type.is_map();

        let children = input.children();

        let file_name: &str = &input.user_data().get_source_file_name();

        let mut last_ty = starting_type;

        let mut values = vec![];

        for value in children {
            let value_span = value.as_span();
            let value = Self::value(value)?;
            let value_ty = value.for_type().to_err_vec()?;

            let Some(expected_type_for_value) = last_ty.supports_index() else {
                return Err(vec![new_err(
                    value_span,
                    file_name,
                    format!("`{last_ty}` does not support indexing, yet here `{value_ty}` is used"),
                )]);
            };

            if !expected_type_for_value
                .contains(&value, value_span, file_name)
                .to_err_vec()?
            {
                return Err(vec![new_err(value_span, file_name, format!("`{last_ty}` does not support indexing for `{value_ty}` (Hint: this type does support indexes of types {expected_type_for_value})"))]);
            }

            let index_output_ty = map_err(
                last_ty.get_output_type_from_index(&value, &TypecheckFlags::use_class(input.user_data().get_type_of_executing_class())),
                value_span,
                file_name,
                format!("invalid index into `{last_ty}`"),
            )
            .to_err_vec()?;

            last_ty = index_output_ty.into_owned();
            values.push(value);
        }

        Ok(Index {
            origin_is_map,
            parts: values.into_boxed_slice(),
            final_output_type: last_ty,
        })
    }
}
