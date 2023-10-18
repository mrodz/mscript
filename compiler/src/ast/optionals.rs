use std::borrow::Cow;

use crate::{
    ast::{new_err, r#type::IntoType, Dependency},
    instruction,
    parser::{Node, Parser},
    VecErr,
};

use super::{Compile, Dependencies, Ident, TypeLayout, Value};

#[derive(Debug)]
pub(crate) struct UnwrapExpr {
    ident: Ident,
    value: Box<Value>,
}

#[derive(Debug)]
pub(crate) enum Unwrap {
    Fallible {
        value: Box<Value>,
        span: String,
    },
    Infallible {
        value: Box<Value>,
        fallback: Box<Value>,
    },
}

impl Unwrap {
    pub fn value(&self) -> &Value {
        match self {
            Self::Fallible { value, .. } => value.as_ref(),
            Self::Infallible { value, .. } => value.as_ref(),
        }
    }
}

impl Compile for Unwrap {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
        match self {
            Self::Fallible { value, span } => {
                let mut value_compiled = value.compile(state)?;

                value_compiled.push(instruction!(unwrap span));

                Ok(value_compiled)
            }
            Self::Infallible { value, fallback } => {
                let mut value_compiled = value.compile(state)?;

                let mut fallback_compiled = fallback.compile(state)?;

                let instructions_to_skip = fallback_compiled.len() + 1;

                value_compiled.push(instruction!(jmp_not_nil instructions_to_skip));

                value_compiled.append(&mut fallback_compiled);

                Ok(value_compiled)
            }
        }
    }
}

impl IntoType for Unwrap {
    fn for_type(&self) -> anyhow::Result<super::TypeLayout> {
        let ty = self.value().for_type()?;

        if let (true, unwrapped_ty) = ty.is_optional() {
            if let Some(type_exists) = unwrapped_ty {
                return Ok(type_exists.clone().into_owned());
            } else {
                return Ok(TypeLayout::Optional(None));
            }
        }

        Ok(ty)
    }
}

impl Dependencies for Unwrap {
    fn dependencies(&self) -> Vec<Dependency> {
        self.value().net_dependencies()
    }

    fn supplies(&self) -> Vec<Dependency> {
        self.value().supplies()
    }
}

impl Dependencies for UnwrapExpr {
    fn supplies(&self) -> Vec<super::Dependency> {
        vec![Dependency::new(Cow::Borrowed(&self.ident))]
    }

    fn dependencies(&self) -> Vec<Dependency> {
        self.value.net_dependencies()
    }
}

impl Compile for UnwrapExpr {
    fn compile(
        &self,
        state: &super::CompilationState,
    ) -> anyhow::Result<Vec<super::CompiledItem>, anyhow::Error> {
        let mut value_compiled = self.value.compile(state)?;

        let name = self.ident.name();

        value_compiled.push(instruction!(unwrap_into name));

        Ok(value_compiled)
    }
}

impl Parser {
    pub fn unwrap(input: Node) -> Result<Unwrap, Vec<anyhow::Error>> {
        let mut children = input.children();

        let value_node = children.next().unwrap();
        let value_span = value_node.as_span();
        let (line, col) = value_span.start_pos().line_col();
        let value = Self::value(value_node)?;

        if let Some(fallback) = children.next() {
            let fallback = Self::value(fallback)?;

            return Ok(Unwrap::Infallible {
                value: Box::new(value),
                fallback: Box::new(fallback),
            });
        }

        let span = format!("{}:{line}:{col}", &input.user_data().get_source_file_name(),);

        Ok(Unwrap::Fallible {
            value: Box::new(value),
            span,
        })
    }

    pub fn unwrap_expr(input: Node) -> Result<UnwrapExpr, Vec<anyhow::Error>> {
        let mut children = input.children();

        let ident = children.next().unwrap();
        let value = children.next().unwrap();

        let value_span = value.as_span();

        let mut ident = Self::ident(ident).to_err_vec()?;

        let value = Self::value(value)?;

        let value_ty = value.for_type().to_err_vec()?;

        let user_data = input.user_data();

        let optional_check = value_ty.is_optional();

        match optional_check {
            (true, Some(known_type)) => ident
                .link_force_no_inherit(user_data, known_type.clone())
                .to_err_vec()?,
            (true, _) => {
                return Err(vec![new_err(
                    value_span,
                    &user_data.get_source_file_name(),
                    "the type of this optional is unknown".to_owned(),
                )])
            }
            _ => ident
                .link_force_no_inherit(user_data, Cow::Owned(value_ty))
                .to_err_vec()?,
        }

        Ok(UnwrapExpr {
            ident,
            value: Box::new(value),
        })
    }
}
