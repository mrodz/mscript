use std::{borrow::Cow, fmt::Debug};

use anyhow::{bail, Context, Result};

use crate::{
    ast::CompiledItem,
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    map_err, new_err,
    value::{Value, ValueChain},
    Compile, Dependencies, Dependency, Ident,
};

#[derive(Debug)]
pub(crate) struct Assignment {
    pub ident: Ident,
    pub value: ValueChain,
    pub flags: AssignmentFlag,
}

impl Assignment {
    pub fn new(ident: Ident, value: ValueChain) -> Self {
        Self {
            ident,
            value,
            flags: AssignmentFlag::default(),
        }
    }

    pub fn can_modify_if_applicable(
        &self,
        user_data: &AssocFileData,
        is_modify: bool,
    ) -> Result<bool> {
        // println!("wooooo!!!!");
        let skip = if is_modify { 1 } else { 0 };

        if self.flags.contains(AssignmentFlag::modify()) {
            let (ident, _) = user_data
                .get_dependency_flags_from_name_skip_n(self.ident.name(), skip)
                .context(
                    "attempting to look up a variable that does not exist in any parent scope",
                )?;

            return Ok(!ident.is_const());
        }

        let has_been_declared = user_data.get_ident_from_name_local(self.ident.name());

        Ok(has_been_declared.map_or_else(|| true, |ident| !ident.is_const()))
    }

    pub fn set_flags(&mut self, flags: AssignmentFlag) {
        self.flags = flags;
    }
}

impl Dependencies for Assignment {
    fn supplies(&self) -> Vec<Dependency> {
        if !self.flags.contains(AssignmentFlag::modify()) {
            vec![Dependency::new(Cow::Borrowed(&self.ident))]
        } else {
            // We are not introducing a new variable, just pointing to a callback variable.
            // This means we shouldn't count child dependencies as filled by this assignment.
            vec![]
        }
    }

    fn dependencies(&self) -> Vec<Dependency> {
        self.value.net_dependencies()
    }

    /// custom implementation
    fn net_dependencies(&self) -> Vec<Dependency> {
        let dependencies = self.dependencies();

        let Some(supply_name) = self.supplies().pop() else {
            return dependencies;
        };

        let mut result = Vec::with_capacity(dependencies.len());

        for dependency in dependencies {
            if dependency != supply_name {
                result.push(dependency);
            }
        }

        result
    }
}

impl Compile for Assignment {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        let name = self.ident.name();

        let value_chain = &self.value;

        let mut value_init = match &value_chain.0 {
            Value::Ident(ident) => ident.compile(function_buffer)?,
            Value::Function(function) => function.in_place_compile_for_value(function_buffer)?,
            Value::Number(number) => number.compile(function_buffer)?,
            Value::String(string) => string.compile(function_buffer)?,
            Value::MathExpr(math_expr) => math_expr.compile(function_buffer)?,
            Value::Callable(callable) => callable.compile(function_buffer)?,
            Value::Boolean(boolean) => boolean.compile(function_buffer)?,
            Value::List(list) => list.compile(function_buffer)?,
        };

        if let Some(ref next) = value_chain.1 {
            value_init.append(&mut next.compile(function_buffer)?);
        }

        let store_instruction = if self.flags.contains(AssignmentFlag::modify()) {
            instruction!(store_object name)
        } else {
            instruction!(store name)
        };

        value_init.push(store_instruction);

        Ok(value_init)
    }
}

mod assignment_flag {
    pub const MODIFY: u8 = 0b00000001;
    pub const CONST: u8 = 0b00000010;
}

#[derive(Clone, Copy, Default)]
pub struct AssignmentFlag(u8);

impl Debug for AssignmentFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut flagc = 0;

        write!(f, "<")?;

        if self.contains(AssignmentFlag::modify()) {
            write!(f, "modify")?;
            flagc += 1;
        }

        if self.contains(AssignmentFlag::constant()) {
            if flagc > 0 {
                write!(f, ", ")?;
            }
            write!(f, "const")?;
            flagc += 1;
        }

        if flagc == 0 {
            write!(f, "NONE")?;
        }

        write!(f, ">")?;

        Ok(())
    }
}

impl AssignmentFlag {
    pub(crate) const fn modify() -> Self {
        Self(assignment_flag::MODIFY)
    }
    pub(crate) const fn constant() -> Self {
        Self(assignment_flag::CONST)
    }

    pub(crate) fn add_flag(&mut self, flag: AssignmentFlag) -> Result<()> {
        if self.0 & flag.0 == flag.0 {
            bail!("duplicate flags")
        } else {
            self.0 |= flag.0;
            Ok(())
        }
    }

    pub(crate) const fn contains(&self, flag: AssignmentFlag) -> bool {
        self.0 & flag.0 == flag.0
    }

    fn validate(&self) -> Result<()> {
        use self::assignment_flag::*;

        if self.contains(AssignmentFlag(CONST | MODIFY)) {
            bail!("const is an invalid qualifier when mixed with modify in the same assignment")
        }

        Ok(())
    }
}

impl From<&str> for AssignmentFlag {
    fn from(value: &str) -> Self {
        match value {
            "modify" => Self::modify(),
            "const" => Self::constant(),
            other => unimplemented!("{other}"),
        }
    }
}

impl Parser {
    pub fn assignment_flags(input: Node) -> Result<AssignmentFlag> {
        let flags = input.children();

        let mut result = AssignmentFlag::default();

        for flag in flags {
            result.add_flag(flag.as_str().into())?;
        }

        map_err(
            result.validate(),
            input.as_span(),
            &input.user_data().get_source_file_name(),
            "bad assignment flags".into(),
        )?;

        Ok(result)
    }

    pub fn assignment(input: Node) -> Result<Assignment, Vec<anyhow::Error>> {
        let mut children = input.children();

        let maybe_flags_or_assignment = children.next().unwrap();

        // this is guaranteed to be the Node of the flags if the assignment has flags.
        let flags_span = maybe_flags_or_assignment.as_span();

        let (flags, assignment) = if maybe_flags_or_assignment.as_rule() == Rule::assignment_flags {
            (
                Some(Self::assignment_flags(maybe_flags_or_assignment).to_err_vec()?),
                children.next().unwrap(),
            )
        } else {
            (None, maybe_flags_or_assignment)
        };

        let is_const = flags
            .as_ref()
            .map(|flags| flags.contains(AssignmentFlag::constant()))
            .unwrap_or(false);
        let is_modify = flags
            .as_ref()
            .map(|flags| flags.contains(AssignmentFlag::modify()))
            .unwrap_or(false);

        let assignment_span = assignment.as_span();

        let user_data = input.user_data();

        let (mut x, did_exist_before) = match assignment.as_rule() {
            Rule::assignment_no_type => Self::assignment_no_type(assignment, is_const)?,
            Rule::assignment_type => Self::assignment_type(assignment, is_const)?,
            rule => unreachable!("{rule:?}"),
        };

        let ident_ty = x.ident.ty().unwrap();
        if let Some(list_type) = ident_ty.is_list() {
            if list_type.must_be_const() && !is_const {
                return Err(vec![new_err(
                    assignment_span,
                    &user_data.get_source_file_name(),
                    "mixed-type arrays must be const in order to ensure type safety".to_owned(),
                )]);
            }
        }

        if let Some(flags) = flags {
            x.set_flags(flags)
        }

        let requires_check = did_exist_before || is_modify;

        let can_modify_if_applicable = map_err(
            x.can_modify_if_applicable(user_data, is_modify),
            flags_span,
            &user_data.get_source_file_name(),
            "this assignment contains the \"modify\" attribute, which is used to mutate a variable from a higher scope".to_owned(),
        ).to_err_vec()?;

        if requires_check && !can_modify_if_applicable {
            return Err(vec![new_err(
                assignment_span,
                &input.user_data().get_source_file_name(),
                format!(
                    "cannot mutate \"{}\", which is a const variable",
                    x.ident.name()
                ),
            )]);
        }

        Ok(x)
    }
}
