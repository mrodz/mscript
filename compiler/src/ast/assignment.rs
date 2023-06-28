use std::borrow::Cow;

use anyhow::{bail, Context, Result};

use crate::{
    ast::CompiledItem,
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
};

use super::{map_err, new_err, value::Value, Compile, Dependencies, Dependency, Ident};

#[derive(Debug)]
pub(crate) struct Assignment {
    pub ident: Ident,
    pub value: Value,
    pub flags: AssignmentFlags,
}

impl Assignment {
    pub fn new(ident: Ident, value: Value) -> Self {
        Self {
            ident,
            value,
            flags: AssignmentFlags::default(),
        }
    }

    pub fn can_modify_if_applicable(
        &self,
        user_data: &AssocFileData,
        is_modify: bool,
    ) -> Result<bool> {
        // println!("wooooo!!!!");
        let skip = if is_modify { 1 } else { 0 };

        if self.flags.contains(&AssignmentFlag::Modify) {
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

    pub fn set_flags(&mut self, flags: AssignmentFlags) {
        self.flags = flags;
    }
}

impl Dependencies for Assignment {
    fn supplies(&self) -> Vec<Dependency> {
        if !self.flags.contains(&AssignmentFlag::Modify) {
            vec![Dependency::new(Cow::Borrowed(&self.ident))]
        } else {
            // We are not introducing a new variable, just pointing to a callback variable.
            // This means we shouldn't count child dependencies as filled by this assignment. 
            vec![]
        }
    }

    fn dependencies(&self) -> Vec<Dependency> {
        let value = &self.value as &dyn Dependencies;
        value.net_dependencies()
    }
}

impl Compile for Assignment {
    fn compile(&self, function_buffer: &mut Vec<CompiledItem>) -> Result<Vec<super::CompiledItem>> {
        let name = self.ident.name();

        let mut value_init = match &self.value {
            Value::Ident(ident) => {
                vec![instruction!(load ident)]
            }
            Value::Function(function) => function.in_place_compile_for_value(function_buffer)?,
            Value::Number(number) => number.compile(function_buffer)?,
            Value::String(string) => string.compile(function_buffer)?,
            Value::MathExpr(math_expr) => math_expr.compile(function_buffer)?,
            Value::Callable(callable) => callable.compile(function_buffer)?,
            Value::Boolean(boolean) => boolean.compile(function_buffer)?,
        };

        let store_instruction = if self.flags.contains(&AssignmentFlag::Modify) {
            instruction!(store_object name)
        } else {
            instruction!(store name)
        };

        value_init.push(store_instruction);

        Ok(value_init)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentFlag {
    Modify,
    Const,
}

#[derive(Debug, Default)]
pub struct AssignmentFlags(Vec<AssignmentFlag>);

impl AssignmentFlags {
    pub(crate) fn add_flag(&mut self, flag: AssignmentFlag) -> Result<()> {
        if self.0.contains(&flag) {
            bail!("duplicate flags")
        } else {
            self.0.push(flag);
            Ok(())
        }
    }

    pub(crate) fn contains(&self, flag: &AssignmentFlag) -> bool {
        self.0.contains(flag)
    }
}

impl From<&str> for AssignmentFlag {
    fn from(value: &str) -> Self {
        match value {
            "modify" => Self::Modify,
            "const" => Self::Const,
            other => unimplemented!("{other}"),
        }
    }
}

impl Parser {
    pub fn assignment_flags(input: Node) -> Result<AssignmentFlags> {
        let flags = input.children();

        let mut result = AssignmentFlags::default();

        for flag in flags {
            result.add_flag(flag.as_str().into())?;
        }

        Ok(result)
    }

    pub fn assignment(input: Node) -> Result<Assignment> {
        let mut children = input.children();

        let maybe_flags_or_assignment = children.next().unwrap();

        // this is guaranteed to be the Node of the flags if the assignment has flags.
        let flags_span = maybe_flags_or_assignment.as_span();

        let (flags, assignment) = if maybe_flags_or_assignment.as_rule() == Rule::assignment_flags {
            (
                Some(Self::assignment_flags(maybe_flags_or_assignment)?),
                children.next().unwrap(),
            )
        } else {
            (None, maybe_flags_or_assignment)
        };

        let is_const = flags
            .as_ref()
            .map(|flags| flags.contains(&AssignmentFlag::Const))
            .unwrap_or(false);
        let is_modify = flags
            .as_ref()
            .map(|flags| flags.contains(&AssignmentFlag::Modify))
            .unwrap_or(false);

        let assignment_span = assignment.as_span();

        let user_data = input.user_data();

        let (mut x, did_exist_before) = match assignment.as_rule() {
            Rule::assignment_no_type => Self::assignment_no_type(assignment, is_const)?,
            Rule::assignment_type => Self::assignment_type(assignment, is_const)?,
            rule => unreachable!("{rule:?}"),
        };

        if let Some(flags) = flags {
            x.set_flags(flags)
        }

        let requires_check = did_exist_before || is_modify;

        let can_modify_if_applicable = map_err(
            x.can_modify_if_applicable(user_data, is_modify),
            flags_span,
            &user_data.get_source_file_name(),
            "this assignment contains the \"modify\" attribute, which is used to mutate a variable from a higher scope".to_owned(),
        )?;

        if requires_check && !can_modify_if_applicable {
            bail!(new_err(
                assignment_span,
                &input.user_data().get_source_file_name(),
                format!(
                    "cannot mutate \"{}\", which is a const variable",
                    x.ident.name()
                )
            ));
        }

        Ok(x)
    }
}
