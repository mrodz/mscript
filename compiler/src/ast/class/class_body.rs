use std::{borrow::Cow, sync::Arc};

use anyhow::Result;

use crate::{
    ast::{get_net_dependencies, new_err, r#type::IntoType, Dependencies, Dependency, Ident, class::WalkForType},
    parser::{Node, Parser, Rule},
};

use super::class_feature::ClassFeature;

#[derive(Debug)]
pub(crate) struct ClassBody(Vec<ClassFeature>);

impl ClassBody {
    pub fn new(features: Vec<ClassFeature>) -> Self {
        Self(features)
    }

    pub fn get_members(input: &Node) -> Result<Arc<[Ident]>> {
        let mut features = input.children();

        let mut fields = vec![];

        for member in features {
            let ty = ClassFeature::type_from_node(&member)?;
            fields.push(ty);
        }

        Ok(fields.into())
    }

    pub fn into_idents(&self) -> Vec<Ident> {
        use ClassFeature as CF;
        let mut result = vec![];

        for feature in &self.0 {
            match feature {
                CF::Constructor(constructor) => {
                    let constructor_type = constructor.for_type().unwrap();

                    let ident = Ident::new(
                        "$constructor".to_owned(),
                        Some(Cow::Owned(constructor_type)),
                        true,
                    );

					result.push(ident);
                }
				CF::Function(function) => result.push(function.ident().to_owned()),
				CF::Variable(variable) => result.push(variable.ident().to_owned()),
            }
        }

		result
    }
}

impl Dependencies for ClassBody {
    fn supplies(&self) -> Vec<Dependency> {
        self.0.iter().flat_map(|x| x.supplies()).collect()
    }

    fn dependencies(&self) -> Vec<Dependency> {
        let block_dependencies = self.0.iter().flat_map(|x| x.net_dependencies()).collect();

        block_dependencies
    }

    fn net_dependencies(&self) -> Vec<Dependency> {
        get_net_dependencies(self, true)
    }
}

impl Parser {
    pub fn class_body(input: Node) -> Result<ClassBody, Vec<anyhow::Error>> {
        let class_features_node = input.children();

        let mut has_constructor = false;

        let mut class_features = vec![];

        for feature_node in class_features_node {
            use ClassFeature as CF;
            match feature_node.as_rule() {
                Rule::class_constructor => {
                    if !has_constructor {
                        has_constructor = true;
                    } else {
                        return Err(vec![new_err(
                            feature_node.as_span(),
                            &input.user_data().get_file_name(),
                            "Cannot have multiple constructors in a class".to_owned(),
                        )]);
                    }

                    class_features.push(CF::Constructor(Self::constructor(feature_node)?));
                }
                Rule::class_bound_function => {
                    class_features.push(CF::Function(Self::class_bound_function(feature_node)?));
                }
                Rule::class_variable => {
                    class_features.push(CF::Variable(Self::class_variable(feature_node)?));
                }
                rule => unreachable!("{rule:?} is not implemented for class"),
            }
        }

        Ok(ClassBody::new(class_features))
    }
}
