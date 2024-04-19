use anyhow::Result;

use crate::{
    instruction,
    parser::{Node, Parser},
    VecErr,
};

use super::{new_err, Compile, Dependencies, IntoType, TypeLayout, TypecheckFlags, Value};

#[derive(Debug)]
pub(crate) struct MapInitializer {
    map: Vec<(Value, Value)>,
}

#[derive(Debug)]
pub(crate) struct Map {
    ty: MapType,
    initializer: MapInitializer,
}

impl IntoType for Map {
    fn for_type(&self) -> Result<TypeLayout> {
        Ok(TypeLayout::Map(self.ty.clone()))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct MapType {
    key_type: Box<TypeLayout>,
    value_type: Box<TypeLayout>,
}

impl MapType {
    pub(crate) fn new(key_type: Box<TypeLayout>, value_type: Box<TypeLayout>) -> Self {
        Self {
            key_type,
            value_type,
        }
    }

    pub(crate) fn key_type(&self) -> &TypeLayout {
        &self.key_type
    }

    pub(crate) fn value_type(&self) -> &TypeLayout {
        &self.value_type
    }
}

impl Dependencies for Map {
    fn dependencies(&self) -> Vec<super::Dependency> {
        let mut result = vec![];
        for (key, value) in &self.initializer.map {
            result.append(&mut key.net_dependencies());
            result.append(&mut value.net_dependencies());
        }
        result
    }
}

impl Compile for Map {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> Result<Vec<super::CompiledItem>, anyhow::Error> {
        Ok(if self.initializer.map.is_empty() {
            vec![instruction!(make_map)]
        } else {
            let map_register = state.poll_temporary_register();
            let key_register = state.poll_temporary_register();

            let mut result = vec![
                instruction!(make_map(self.initializer.map.len())),
                instruction!(store_fast map_register),
            ];

            for (key, value) in &self.initializer.map {
                result.append(&mut key.compile(state)?);

                result.push(instruction!(store_fast key_register));

                result.append(&mut value.compile(state)?);

                result.push(instruction!(fast_map_insert map_register key_register));
            }

            result.push(instruction!(load_fast map_register));

            result
        })
    }
}

impl Parser {
    pub fn map_type(input: Node) -> Result<MapType> {
        let mut children = input.children();
        let key_node = children.next().unwrap();
        let key_span = key_node.as_span();
        let value_node = children.next().unwrap();

        let key_type = Self::r#type(key_node)?;

        if key_type.is_map() {
            return Err(new_err(
                key_span,
                &input.user_data().get_source_file_name(),
                "maps are not allowed as keys".to_owned(),
            ));
        }

        let value_type = Self::r#type(value_node)?;

        Ok(MapType::new(
            key_type.into_owned().into(),
            value_type.into_owned().into(),
        ))
    }

    pub fn map_initializer(
        input: Node,
        map_type: &MapType,
    ) -> Result<Vec<(Value, Value)>, Vec<anyhow::Error>> {
        let mut errors = vec![];
        let mut result = vec![];

        for kv_pair in input.children() {
            let mut children = kv_pair.children();
            let key_node = children.next().unwrap();
            let key_span = key_node.as_span();
            let value_node = children.next().unwrap();
            let value_span = value_node.as_span();
            let k = Self::value(key_node);
            let v = Self::value(value_node);

            match (k, v) {
                (Err(mut k_errors), Err(mut v_errors)) => {
                    errors.append(&mut k_errors);
                    errors.append(&mut v_errors);
                }
                (Ok(..), Err(mut some_errors)) | (Err(mut some_errors), Ok(..)) => {
                    errors.append(&mut some_errors)
                }
                (Ok(key), Ok(value)) => {
                    let maybe_class_type = {
                        input
                            .user_data()
                            .get_type_of_executing_class()
                            .map(|x| x.clone())
                    };

                    let key_type = key
                        .for_type(&TypecheckFlags::use_class(maybe_class_type.as_ref()))
                        .unwrap();

                    if !key_type.eq_complex(
                        map_type.key_type(),
                        &TypecheckFlags::use_class(maybe_class_type.as_ref()),
                    ) {
                        errors.push(new_err(key_span, &input.user_data().get_source_file_name(), format!("This map expects keys with type `{}`, but instead found type `{key_type}`", map_type.key_type())))
                    }

                    let value_type = value
                        .for_type(&TypecheckFlags::use_class(maybe_class_type.as_ref()))
                        .unwrap();

                    if !value_type.eq_complex(
                        map_type.value_type(),
                        &TypecheckFlags::use_class(maybe_class_type.as_ref()),
                    ) {
                        errors.push(new_err(value_span, &input.user_data().get_source_file_name(), format!("This map expects values with type `{}`, but instead found type `{value_type}`", map_type.value_type())))
                    }

                    result.push((key, value));
                }
            }
        }

        if errors.is_empty() {
            Ok(result)
        } else {
            Err(errors)
        }
    }

    pub fn map(input: Node) -> Result<Map, Vec<anyhow::Error>> {
        let mut children = input.children();
        let ty = Self::map_type(children.next().unwrap()).to_err_vec()?;

        let initializer = if let Some(next) = children.next() {
            Self::map_initializer(next, &ty)?
        } else {
            vec![]
        };

        Ok(Map {
            initializer: MapInitializer { map: initializer },
            ty,
        })
    }
}
