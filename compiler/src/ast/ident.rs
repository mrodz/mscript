use std::borrow::Cow;
use std::fmt::Display;
use std::hash::Hash;

use anyhow::{bail, Context, Result};

use crate::parser::{AssocFileData, Node, Parser};

use super::r#type::TypeLayout;
use super::{Compile, CompiledItem, Dependencies, Dependency};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Ident {
    name: String,
    ty: Option<Cow<'static, TypeLayout>>,
}

impl Compile for Ident {
    fn compile(&self) -> Result<Vec<CompiledItem>> {
        let ty = self.ty()?;
        let (_, id) = TypeLayout::get_load_instruction(&ty);

        Ok(vec![CompiledItem::Instruction {
            id,
            arguments: Box::new([self.name.clone()]),
        }])
    }
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

impl Ident {
    pub fn lookup(&mut self, user_data: &AssocFileData) -> &mut Self {
        if let Some(ref ty) = self.ty {
            panic!("already has type {ty:?}")
        }

        let (ident, is_callback) = user_data
            .get_dependency_flags_from_name(self.name.clone())
            .expect("variable has not been mapped");

        let ty = ident.ty.clone();

        let x = ty.map(|x| {
            if is_callback {
                Cow::Owned(TypeLayout::CallbackVariable(x.into_owned().into()))
            } else {
                x.to_owned()
            }
        });

        let ty = x.unwrap();

        self.ty = Some(ty);
        // let ty = ident.ty().unwrap().to_owned(); //.expect("trying to inherit nothing instead of a type").clone();

        // // let ty = ty.unwrap();

        // // let ty = ty.clone();
        // // let ty = ty.to_owned();/

        // ident.ty = Some(ty.clone());

        self
        // Ok(ty.clone())
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn ty(&self) -> Result<&Cow<TypeLayout>> {
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

impl Dependencies for Ident {
    fn get_dependencies(&self) -> Option<Box<[super::Dependency]>> {
        Some(Box::new([Dependency::new(Cow::Borrowed(self))]))
    }
}

impl Ident {
    #[allow(dead_code)]
    pub fn load_type(&mut self, user_data: &AssocFileData) -> Result<()> {
        let (ident, _) = user_data
            .get_dependency_flags_from_name(self.name.clone())
            .context("variable has not been mapped")?;

        self.ty = Some(ident.ty.clone().context("no type")?);

        Ok(())
    }

    pub fn link_from_pointed_type_with_lookup(&mut self, user_data: &AssocFileData) -> Result<()> {
        if let Some(ref ty) = self.ty {
            bail!("already has type {ty:?}")
        }

        let (ident, is_callback) = user_data
            .get_dependency_flags_from_name(self.name.clone())
            .context("variable has not been mapped")?;

        let new_ty = ident.ty.clone().map(|x| {
            if is_callback {
                Cow::Owned(TypeLayout::CallbackVariable(x.into_owned().into()))
                // Cow::Owned(TypeLayout::CallbackVariable(callback_variable));
                // Cow::Owned(TypeLayout::CallbackVariable(AsRef::<&'static TypeLayout>::as_ref(x)))
            } else {
                x.to_owned()
            }
        });

        self.ty = new_ty;

        Ok(())
        // Ok(ty.clone())
    }

    pub fn link(
        &mut self,
        user_data: &AssocFileData,
        ty: Option<Cow<'static, TypeLayout>>,
    ) -> Result<bool> {
        let ident = user_data.get_dependency_flags_from_name(self.name.clone());

        let inherited = if let Some((ident, is_callback)) = ident {
            let new_ty = ident.ty.clone().map(|x| {
                if is_callback {
                    Cow::Owned(TypeLayout::CallbackVariable(x.into_owned().into()))
                } else {
                    x.to_owned()
                }
            });

            self.ty = new_ty;
            // self.ty = Some(ident.ty.clone().context("no type")?);

            true
        } else {
            if ty.is_none() {
                bail!("ident has not already been registered and needs a type",)
            }

            self.ty = ty;

            false
        };

        user_data.add_dependency(self)?;

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
