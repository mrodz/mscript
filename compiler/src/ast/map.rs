use std::{borrow::Cow, collections::HashMap};

use anyhow::Result;

use crate::{parser::{Node, Parser}, VecErr};

use super::{Compile, Dependencies, IntoType, TypeLayout, Value};

#[derive(Debug)]
pub(crate) struct MapInitializer {
    map: HashMap<Value, Value>,
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
        todo!()
    }
}

impl Parser {
    pub fn map_type(input: Node) -> Result<TypeLayout> {
        let mut children = input.children();
        let key_node = children.next().unwrap();
        let value_node = children.next().unwrap();

        let key_type = Self::r#type(key_node)?;
        let value_type = Self::r#type(value_node)?;

        Ok(TypeLayout::Map(MapType::new(
            key_type.into_owned().into(),
            value_type.into_owned().into(),
        )))
    }

    pub fn map(input: Node) -> Result<Map, Vec<anyhow::Error>> {
        let mut children = input.children();
        let map_type = Self::map_type(children.next().unwrap()).to_err_vec()?;

        
        todo!()
    }
}
