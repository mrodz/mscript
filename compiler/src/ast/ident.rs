use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::Hash;

use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;

use crate::instruction;
use crate::parser::{AssocFileData, Node, Parser, Rule};

use super::r#type::TypeLayout;
use super::{new_err, CompilationState, Compile, CompiledItem, Dependencies, Dependency};

#[derive(Debug, Clone, Eq)]
pub(crate) struct Ident {
    name: String,
    ty: Option<Cow<'static, TypeLayout>>,
    read_only: bool,
}

impl Compile for Ident {
    fn compile(&self, _: &CompilationState) -> Result<Vec<CompiledItem>> {
        Ok(vec![instruction!(load(self.name()))])
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
        "fn",
        "obj",
        "print",
        "return",
        "if",
        "else",
        "true",
        "false",
        "modify",
        "const",
        "self",
        "while",
        "continue",
        "break",
        "from",
        "constructor",
        "class",
        "Self",
        "get",
        "or",
        "xor",
        "nil",
        "is",
        "map",
    ])
});

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.name)?;
        if let Some(ref ty) = self.ty {
            write!(f, "{ty}")
        } else {
            write!(f, "!!!")
        }
    }
}

impl Dependencies for Ident {
    fn dependencies(&self) -> Vec<Dependency> {
        vec![Dependency::new(Cow::Borrowed(self))]
    }
}

impl Ident {
    pub const fn new(name: String, ty: Option<Cow<'static, TypeLayout>>, read_only: bool) -> Self {
        Self {
            name,
            ty,
            read_only,
        }
    }

    pub fn mark_const(&mut self) {
        self.read_only = true;
    }

    pub fn is_const(&self) -> bool {
        self.read_only
    }

    pub fn is_instance_callback_variable(&self) -> Result<bool> {
        Ok(self.ty()?.is_directly_callback_variable())
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

    pub(crate) fn clone_with_type(&self, ty: Cow<'static, TypeLayout>) -> Self {
        Self {
            name: self.name.clone(),
            read_only: self.read_only,
            ty: Some(ty),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn boxed_name(&self) -> Box<str> {
        self.name.clone().into_boxed_str()
    }

    pub fn ty_owned(self) -> Result<Cow<'static, TypeLayout>> {
        if let Some(ty) = self.ty {
            Ok(ty)
        } else {
            bail!("trying to get the type of an identifier that is typeless")
        }
    }

    pub fn ty(&self) -> Result<&Cow<'static, TypeLayout>> {
        if let Some(ref ty) = self.ty {
            Ok(ty)
        } else {
            bail!("trying to get the type of an identifier that is typeless")
        }
    }

    pub fn set_type_no_link(&mut self, ty: Cow<'static, TypeLayout>) {
        self.ty = Some(ty);
    }

    pub fn link_from_pointed_type_with_lookup(&mut self, user_data: &AssocFileData) -> Result<()> {
        if let Some(ref ty) = self.ty {
            bail!("already has type {ty:?}")
        }

        let (ident, _) = user_data
            .get_dependency_flags_from_name(&self.name)
            .with_context(|| format!("'{}' has not been mapped", self.name))?;

        let new_ty = ident.ty().expect("no type").get_type_recursively();
        let new_ty = Cow::Owned(new_ty.clone());

        self.ty = Some(new_ty);

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
}

impl Parser {
    pub fn ident(input: Node) -> Result<Ident> {
        debug_assert_eq!(input.as_rule(), Rule::ident);

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
