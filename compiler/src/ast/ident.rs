use std::borrow::Cow;
use std::cell::Ref;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;

use crate::parser::{AssocFileData, Node, Parser};

use super::r#type::TypeLayout;
use super::{new_err, Compile, CompiledItem, Dependencies, Dependency};

#[derive(Debug, Clone, Eq)]
pub(crate) struct Ident {
    name: String,
    ty: Option<Cow<'static, TypeLayout>>,
    read_only: bool,
}

impl Compile for Ident {
    fn compile(&self, _: &mut Vec<CompiledItem>) -> Result<Vec<CompiledItem>> {
        let ty = self.ty()?;
        let (_, id) = TypeLayout::get_load_instruction(ty);

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

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

pub static KEYWORDS: Lazy<HashSet<&'static str>> = Lazy::new(|| {
    HashSet::from_iter([
        "fn", "obj", "print", "return", "if", "else", "true", "false", "modify", "const", "self", "while", "continue", "break", "from", 
    ])
});



impl Ident {
    pub fn mark_const(&mut self) {
        self.read_only = true;
    }

    pub fn is_const(&self) -> bool {
        self.read_only
    }

    pub fn wrap_in_callback(mut self) -> Result<Self> {
        let Some(ty) = self.ty else {
            bail!("this variable does not have a type; it can't be wrapped in a callback")
        };

        self.ty = Some(Cow::Owned(TypeLayout::CallbackVariable(
            ty.into_owned().into(),
        )));

        Ok(self)
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
    fn dependencies(&self) -> Vec<Dependency> {
        vec![Dependency::new(Cow::Borrowed(self))]
    }
}

impl Ident {
    #[allow(dead_code)]
    pub fn load_type(&mut self, user_data: &AssocFileData) -> Result<()> {
        let (ident, _) = user_data
            .get_dependency_flags_from_name(&self.name)
            .context("variable has not been mapped")?;

        self.ty = Some(ident.ty.clone().context("no type")?);

        Ok(())
    }

    pub fn link_from_pointed_type_with_lookup(&mut self, user_data: &AssocFileData) -> Result<()> {
        if let Some(ref ty) = self.ty {
            bail!("already has type {ty:?}")
        }

        let (ident, is_callback) = user_data
            .get_dependency_flags_from_name(&self.name)
            .with_context(|| format!("'{}' has not been mapped", self.name))?;

        let new_ty = ident.ty.clone().map(|x| {
            if is_callback {
                Cow::Owned(TypeLayout::CallbackVariable(x.into_owned().into()))
            } else {
                x
            }
        });

        self.ty = new_ty;

        Ok(())
    }

    pub fn link_force_no_inherit(
        &mut self,
        user_data: &AssocFileData,
        ty: Cow<'static, TypeLayout>,
    ) -> Result<()> {
        self.ty = Some(ty);
        user_data.add_dependency(self);
        Ok(())
    }

    #[deprecated]
    #[allow(unused)]
    pub fn link(
        &mut self,
        user_data: &AssocFileData,
        ty: Option<Cow<'static, TypeLayout>>,
    ) -> Result<bool> {
        let ident: Option<(Ref<Ident>, bool)> = user_data.get_dependency_flags_from_name(&self.name);

        let inherited: bool = if let Some((ident, is_callback)) = ident {
            let new_ty = ident.ty.clone().map(|x| {
                if is_callback {
                    Cow::Owned(TypeLayout::CallbackVariable(x.into_owned().into()))
                } else {
                    x
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

        user_data.add_dependency(self);

        Ok(inherited)
    }
}

impl Parser {
    pub fn ident(input: Node) -> Result<Ident> {
        let name = input.as_str();

        if KEYWORDS.contains(name) {
            bail!(new_err(
                input.as_span(),
                &input.user_data().get_source_file_name(),
                "this is a reserved keyword".into()
            ));
        }

        let name = name.to_owned();

        Ok(Ident {
            name,
            ty: None,
            read_only: false,
        })
    }
}
