use std::{borrow::Cow, collections::HashMap, fmt::Display, rc::Rc};

use crate::parser::{util::parse_with_userdata, AssocFileData, Node, Parser, Rule};
use anyhow::{bail, Result};
use once_cell::sync::Lazy;

use super::{function::FunctionType, map_err, math_expr::Op, FunctionParameters};

pub(crate) static mut TYPES: Lazy<HashMap<&str, TypeLayout>> = Lazy::new(|| {
    let mut x = HashMap::new();

    x.insert("bool", TypeLayout::Native(NativeType::Bool));
    x.insert("str", TypeLayout::Native(NativeType::Str));
    x.insert("int", TypeLayout::Native(NativeType::Int));
    x.insert("bigint", TypeLayout::Native(NativeType::BigInt));
    x.insert("float", TypeLayout::Native(NativeType::Float));
    x.insert("byte", TypeLayout::Native(NativeType::Byte));

    x
});

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeType {
    Bool,
    Str,
    Int,
    BigInt,
    Float,
    Byte,
}

impl Display for NativeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = format!("{:?}", self).to_ascii_lowercase();
        write!(f, "{name}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeLayout {
    Function(FunctionType),
    /// metadata wrapper around a [TypeLayout]
    CallbackVariable(Box<TypeLayout>),
    Native(NativeType),
}

impl Display for TypeLayout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function_type) => write!(f, "{}", function_type.to_string()),
            Self::CallbackVariable(cb) => write!(f, "{}", cb.get_type_recursively().to_string()),
            Self::Native(native) => write!(f, "{}", native.to_string()),
        }
    }
}

impl TypeLayout {
    pub fn is_function(&self) -> Option<&FunctionType> {
        let me = self.get_type_recursively();

        let TypeLayout::Function(f) = me else {
            return None;
        };

        Some(f)
    }

    pub fn can_negate(&self) -> bool {
        let me = self.get_type_recursively();
        match me {
            Self::Native(NativeType::Str) => false,
            Self::Native(_) => true,
            _ => false,
        }
    }

    pub fn get_type_recursively(&self) -> &Self {
        use TypeLayout::*;

        match self {
            CallbackVariable(cb) => cb.get_type_recursively(),
            _ => self,
        }
    }

    pub fn get_output_type(&self, other: &Self, op: &Op) -> Option<TypeLayout> {
        use TypeLayout::*;

        let Native(me) = self.get_type_recursively() else {
            return None;
        };

        let Native(other) = other.get_type_recursively() else {
            return None
        };

        use NativeType::*;
        use Op::*;

        let matched = match (me, other, op) {
            (Int, Int, ..) => Int,
            (Int, BigInt, ..) => BigInt,
            (Int, Float, ..) => Float,
            //======================
            (Float, Float, ..) => Float,
            (Float, Int, ..) => Float,
            (Float, BigInt, ..) => Float,
            //======================
            (BigInt, BigInt, ..) => BigInt,
            (BigInt, Int, ..) => BigInt,
            (BigInt, Float, ..) => Float,
            //======================
            (x, Byte, ..) => x.clone(), // byte will always get overshadowed.
            //======================
            (Str, Str | Int | BigInt | Float | Bool, Add) => Str,
            (Str, Int | BigInt, Multiply) => Str,
            (Int | BigInt, Str, Multiply) => Str,
            //======================
            _ => return None,
        };

        Some(TypeLayout::Native(matched))
    }

    #[allow(unused)]
    pub fn is_numeric(&self, allow_byte: bool) -> bool {
        let Self::Native(native) = self else {
            return false;
        };

        match native {
            NativeType::Byte if allow_byte => true,
            NativeType::BigInt | NativeType::Int | NativeType::Float => true,
            _ => false,
        }
    }

    pub fn get_load_instruction(&self) -> (&'static str, u8) {
        const LOAD_CALLBACK: u8 = 0x23;
        const LOAD: u8 = 0x19;

        match self {
            Self::CallbackVariable(..) => ("load_callback", LOAD_CALLBACK),
            _ => ("load", LOAD),
        }
    }
}

pub(crate) trait IntoType {
    fn into_type(&self) -> Result<TypeLayout>;
    fn consume_for_type(self) -> Result<TypeLayout>
    where
        Self: Sized,
    {
        self.into_type()
    }
}

pub(crate) fn type_from_str(
    input: &str,
    user_data: Rc<AssocFileData>,
) -> Result<Cow<'static, TypeLayout>> {
    unsafe {
        if let Some(r#type) = TYPES.get(input) {
            Ok(Cow::Borrowed(r#type))
        } else {
            if let Ok(function_type) = parse_with_userdata(Rule::function_type, input, user_data) {
                let single = function_type.single()?;
                let ty = Parser::function_type(single)?;
                return Ok(Cow::Owned(TypeLayout::Function(ty)));
            }

            bail!("type '{input}' has not been registered")
        }
    }
}

impl Parser {
    pub fn function_type(input: Node) -> Result<FunctionType> {
        let mut children = input.children();

        let mut child_and_rule = || {
            let Some(child) = children.next() else {
                return None
            };

            let rule = child.as_rule();

            Some((child, rule))
        };

        let mut types = vec![];

        let mut child_and_rule_pair = child_and_rule();

        while let Some((child, Rule::r#type)) = child_and_rule_pair {
            let ty = Parser::r#type(child)?;
            types.push(ty);

            child_and_rule_pair = child_and_rule();
        }

        let return_type =
            if let Some((return_value, Rule::function_return_type)) = child_and_rule_pair {
                let ty = Parser::function_return_type(return_value)?;
                Some(Box::new(ty))
            } else {
                None
            };

        let types = FunctionParameters::TypesOnly(types);

        Ok(FunctionType::new(types, return_type))
    }

    pub fn r#type(input: Node) -> Result<Cow<'static, TypeLayout>> {
        let as_str = input.as_str();
        let span = input.as_span();
        let file_name = input.user_data().get_file_name();
        map_err(
            type_from_str(as_str, input.into_user_data()),
            span,
            &*file_name,
            "unknown type".into(),
        )
    }
}
