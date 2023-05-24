use std::{fmt::Display, hash::Hash};

use anyhow::{bail, Context, Result};

use crate::parser::{AssocFileData, Node, Parser};

use super::{
    r#type::{IntoType, TypeLayout},
    Dependencies,
};

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub enum IdentType {
//     Simple,
//     Function,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Ident {
    name: String,
    ty: Option<TypeLayout>,
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Ident {
    pub fn new(name: String, ty: TypeLayout) -> Self {
        Self { name, ty: Some(ty) }
    }

    pub fn new_unsafe(name: String, ty: Option<TypeLayout>) -> Self {
        Self { name, ty }
    }

    pub fn set_ty(&mut self, ty: TypeLayout) {
        self.ty = Some(ty);
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn ty(&self) -> Result<&TypeLayout> {
        if let Some(ref ty) = self.ty {
            Ok(ty)
        } else {
            bail!("trying to get the type of an identifier that is typeless")
        }
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.name, self.ty)
    }
}

impl Dependencies for Ident {}

impl Ident {
    pub fn load_type(&mut self, user_data: &AssocFileData) -> Result<()> {
        let ident = user_data
            .get_dependency_flags_from_name(self.name.clone())
            .context("variable has not been mapped")?;

        self.ty = Some(ident.ty()?.clone());

        Ok(())
    }

    pub fn link_from_pointed_type_with_lookup(&mut self, user_data: &AssocFileData) -> Result<()> {
        if let Some(ref ty) = self.ty {
            bail!("already has type {ty:?}")
        }

        let ident = user_data
            .get_dependency_flags_from_name(self.name.clone())
            .context("variable has not been mapped")?;

        self.ty = Some(ident.ty()?.clone());

        Ok(())
        // Ok(ty.clone())
    }

    pub fn link(&mut self, user_data: &AssocFileData, ty: Option<TypeLayout>) -> Result<bool> {
        let ident = user_data.get_dependency_flags_from_name(self.name.clone());

        let inherited = if let Some(ident) = ident {
            self.ty = Some(ident.ty()?.clone());

            true
        } else {
            if ty.is_none() {
                bail!(
                    "ident has not already been registered and needs a type",
                )
            }
            self.ty = ty;

            false
        };

        user_data.add_dependency(self);

        Ok(inherited)
    }
}

impl Parser {
    pub fn ident(input: Node) -> Ident {
        let name = input.as_str().to_owned();

        Ident { name, ty: None }

        // if let Some(ident) = ident {
        //     Cow::Borrowed(ident)
        // } else {
        //     Cow::Owned((Ident { name: as_str, ty: None }, DependencyFlags::default()))
        // }
    }
}
