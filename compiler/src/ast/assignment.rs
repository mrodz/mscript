mod assignment_no_type;
mod assignment_type;
mod assignment_unpack;

use std::{borrow::Cow, fmt::Debug};

use anyhow::{bail, Context, Result};

use crate::{
    instruction,
    parser::{AssocFileData, Node, Parser, Rule},
    VecErr,
};

use super::{
    map_err, new_err, value::Value, CompilationState, Compile, Dependencies, Dependency, Ident,
    TypeLayout, TypecheckFlags, WalkForType,
};

#[derive(Debug)]
pub(crate) struct Assignment {
    idents: Box<[Ident]>,
    value: Value,
    flags: AssignmentFlag,
}

impl WalkForType for Assignment {
    fn type_from_node(input: &Node) -> Result<Ident> {
        assert_eq!(input.as_rule(), Rule::assignment);

        let mut children = input.children();

        let maybe_flags_or_assignment = children.next().unwrap();

        let (flags, assignment) = if maybe_flags_or_assignment.as_rule() == Rule::assignment_flags {
            (
                Some(Parser::assignment_flags(maybe_flags_or_assignment)?),
                children.next().unwrap(),
            )
        } else {
            (None, maybe_flags_or_assignment)
        };

        let Some(flags) = flags else {
            bail!("not an error; no export, disregard")
        };

        if !flags.contains(AssignmentFlag::export()) {
            bail!("not an error; no export, disregard")
        }

        match assignment.as_rule() {
            Rule::assignment_type => {
                let mut children = assignment.children();
                let ident = children.next().unwrap();
                let ty = children.next().unwrap();

                let mut ident = Parser::ident(ident)?;
                ident.link_force_no_inherit(input.user_data(), Parser::r#type(ty)?)?;

                Ok(ident)
            }
            other => bail!("{other:?} does not support static type evaluation"),
        }
    }
}

impl Assignment {
    pub fn new(ident: Ident, value: Value) -> Self {
        Self {
            idents: Box::new([ident]),
            value,
            flags: AssignmentFlag(0),
        }
    }

    pub const fn new_multi(idents: Box<[Ident]>, value: Value) -> Self {
        Self {
            idents,
            value,
            flags: AssignmentFlag(0),
        }
    }

    #[inline]
    pub const fn flags(&self) -> &AssignmentFlag {
        &self.flags
    }

    #[inline]
    pub const fn value(&self) -> &Value {
        &self.value
    }

    pub fn can_modify_if_applicable(
        &self,
        user_data: &AssocFileData,
        is_modify: bool,
    ) -> Result<bool> {
        if self.idents.len() != 1 {
            bail!("cannot modify a pre-existing variable when unpacking");
        };

        let skip = if is_modify { 1 } else { 0 };

        let name = self.idents[0].name();

        if self.flags().contains(AssignmentFlag::modify()) {
            let (ident, _) = user_data
                .get_dependency_flags_from_name_skip_n(name, skip)
                .context(
                    "attempting to look up a variable that does not exist in any parent scope",
                )?;

            return Ok(!ident.is_const());
        }

        let has_been_declared = user_data.get_ident_from_name_local(name);

        Ok(has_been_declared.map_or_else(|| true, |ident| !ident.is_const()))
    }

    pub fn set_flags(&mut self, new_flags: AssignmentFlag) {
        self.flags = new_flags;
    }
}

impl Dependencies for Assignment {
    fn supplies(&self) -> Vec<Dependency> {
        if self.idents.len() == 1 && !self.flags().contains(AssignmentFlag::modify()) {
            return vec![Dependency::new(Cow::Borrowed(&self.idents[0]))];
        }

        // We are not introducing a new variable, just pointing to a callback variable.
        // This means we shouldn't count child dependencies as filled by this assignment.
        // Unpacking a result does not support the modify keyword so it is left out.
        vec![]
    }

    fn dependencies(&self) -> Vec<Dependency> {
        let mut base = self.value().net_dependencies();

        if self.idents.len() == 1 && self.idents[0].is_instance_callback_variable().unwrap() {
            base.push(Dependency::new(Cow::Borrowed(&self.idents[0])));
        }

        base
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
    fn compile(&self, state: &CompilationState) -> Result<Vec<super::CompiledItem>> {
        let mut value_init = self.value().compile(state)?;

        if self.idents.len() == 1 {
            let name = self.idents[0].name();
            let store_instruction = if self.flags().contains(AssignmentFlag::modify()) {
                instruction!(store_object name)
            } else {
                instruction!(store name)
            };

            value_init.push(store_instruction);

            if self.flags.contains(AssignmentFlag::export()) {
                value_init.push(instruction!(export_name name));
            }
        } else {
            let indexable = state.poll_temporary_register();
            value_init.push(instruction!(store_fast indexable));

            for (idx, ident) in self.idents.iter().enumerate() {
                let name = ident.name();

                value_init.append(&mut vec![
                    instruction!(load_fast indexable),
                    instruction!(vec_op(format!("[{idx}]"))),
                    instruction!(store name),
                ])
            }

            // value_init.push(instruction!(delete_name_scoped indexable));
        }

        Ok(value_init)
    }
}

mod assignment_flag {
    pub const MODIFY: u8 = 0b00000001;
    pub const CONST: u8 = 0b00000010;
    pub const EXPORT: u8 = 0b00000100;
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

        if self.contains(AssignmentFlag::export()) {
            write!(f, "export")?;
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
    pub(crate) const fn export() -> Self {
        Self(assignment_flag::EXPORT)
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
            bail!("`const` is an invalid qualifier when mixed with `modify` in the same assignment")
        }

        if self.contains(AssignmentFlag(EXPORT | MODIFY)) {
            bail!(
                "`modify` is an invalid qualifier when mixed with `export` in the same assignment"
            )
        }

        Ok(())
    }
}

impl From<&str> for AssignmentFlag {
    fn from(value: &str) -> Self {
        match value {
            "modify" => Self::modify(),
            "const" => Self::constant(),
            "export" => Self::export(),
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

        let mut span_getter = assignment.children();
        let name_span = span_getter.next().unwrap().as_span();
        let value_span = span_getter.next().unwrap().as_span();

        let is_const = flags
            .as_ref()
            .map(|flags| flags.contains(AssignmentFlag::constant()))
            .unwrap_or(false);
        let is_modify = flags
            .as_ref()
            .map(|flags| flags.contains(AssignmentFlag::modify()))
            .unwrap_or(false);
        let is_export = flags
            .as_ref()
            .map(|flags| flags.contains(AssignmentFlag::export()))
            .unwrap_or(false);

        if is_export && !input.user_data().is_at_module_level() {
            return Err(vec![new_err(
                name_span,
                &input.user_data().get_source_file_name(),
                "exporting variables/constants must occur at the module level".to_owned(),
            )]);
        }

        let user_data = input.user_data();

        // We can't borrow a `Ref` here, because `rvalue` portion might borrow the call stack mutably.
        let self_type = user_data.get_owned_type_of_executing_class();

        let (mut x, did_exist_before) = match assignment.as_rule() {
            Rule::assignment_no_type => {
                if is_export {
                    return Err(vec![new_err(
                        name_span,
                        &input.user_data().get_source_file_name(),
                        "Exports require an explicit type".to_owned(),
                    )]);
                }
                Self::assignment_no_type(assignment, is_const, is_modify)?
            }
            Rule::assignment_type => {
                Self::assignment_type(assignment, is_const, is_modify, self_type.as_ref())?
            }
            Rule::assignment_unpack => {
                if is_export {
                    return Err(vec![new_err(
                        name_span,
                        &input.user_data().get_source_file_name(),
                        "Seperate each name into its own `export NAME = ...`".to_owned(),
                    )]);
                }
                Self::assignment_unpack(assignment, is_const, is_modify)?
            }
            rule => unreachable!("{rule:?}"),
        };

        if matches!(
            x.value
                .for_type(&TypecheckFlags::use_class(
                    input.user_data().get_type_of_executing_class()
                ))
                .unwrap()
                .disregard_distractors(true),
            TypeLayout::Void
        ) {
            return Err(vec![new_err(
                value_span,
                &input.user_data().get_source_file_name(),
                "cannot store void".to_owned(),
            )]);
        }

        if x.idents.len() == 1 {
            let ident_ty = x.idents[0].ty().unwrap();
            if let Some(list_type) = ident_ty.is_list() {
                if list_type.must_be_const() && !is_const {
                    return Err(vec![new_err(
                        name_span,
                        &user_data.get_source_file_name(),
                        "mixed-type arrays must be const in order to ensure type safety\n\t+ hint: if you meant a growable or variable-sized array, use [TYPE...] instead".to_owned(),
                    )]);
                }
            }
        } else if let Some(ident) = did_exist_before {
            return Err(vec![new_err(
                input.as_span(),
                &user_data.get_source_file_name(),
                format!("this unpack operation shadows one or more names accesible from this scope, starting at `{}`", ident.name()),
            )]);
        }

        if let Some(flags) = flags {
            x.set_flags(flags)
        }

        if x.idents.len() == 1 {
            if let Some(previous_ident) = did_exist_before.as_ref() {
                let ident = &x.idents[0];

                if previous_ident.is_const() {
                    return Err(vec![new_err(
                        value_span,
                        &input.user_data().get_source_file_name(),
                        "attempting to reassign to a const variable".to_owned(),
                    )]);
                }

                if !previous_ident.ty().unwrap().eq_complex(
                    ident.ty().unwrap(),
                    &TypecheckFlags::use_class(self_type.as_ref()),
                ) {
                    return Err(vec![new_err(
                    value_span,
                    &input.user_data().get_source_file_name(),
                    format!("type mismatch: this assignment will update a variable with type `{}`, which is not compatible with the original type `{}`", ident.ty().unwrap(), previous_ident.ty().unwrap())
                )]);
                }
            }

            let requires_check = did_exist_before.is_some() || is_modify;

            let can_modify_if_applicable = map_err(
                x.can_modify_if_applicable(user_data, is_modify),
                flags_span,
                &user_data.get_source_file_name(),
                "this assignment contains the \"modify\" attribute, which is used to mutate a variable from a higher scope".to_owned(),
            ).to_err_vec()?;

            if requires_check && !can_modify_if_applicable {
                return Err(vec![new_err(
                    name_span,
                    &input.user_data().get_source_file_name(),
                    format!(
                        "cannot reassign to \"{}\", which is a `const` variable",
                        x.idents[0].name(),
                    ),
                )]);
            }

            if let (Some(previous_ty), true) = (did_exist_before, is_modify) {
                if previous_ty.is_const() {
                    return Err(vec![new_err(
                        name_span,
                        &input.user_data().get_source_file_name(),
                        format!(
                            "cannot modify {}, a captured `const` variable",
                            x.idents[0].name(),
                        ),
                    )]);
                }
                if !previous_ty.ty().unwrap().eq_complex(
                    x.idents[0].ty().unwrap(),
                    &TypecheckFlags::use_class(self_type.as_ref()),
                ) {
                    return Err(vec![new_err(
                        name_span,
                        &input.user_data().get_source_file_name(),
                        format!("type mismatch: this assignment will update a variable with type `{}`, which is not compatible with the original type `{}`", previous_ty.ty().unwrap(), &x.idents[0])
                    )]);
                }
            }
        }

        Ok(x)
    }
}
