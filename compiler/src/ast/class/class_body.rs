use std::sync::Arc;

use anyhow::Result;

use crate::{
    ast::{
        get_net_dependencies, new_err, CompilationState, Compile, CompiledItem, Dependencies,
        Dependency, Ident, WalkForType,
    },
    instruction,
    parser::{Node, Parser, Rule},
};

use super::{class_feature::ClassFeature, Constructor};

#[derive(Debug)]
pub(crate) struct ClassBody {
    features: Vec<ClassFeature>,
    constructor: Constructor,
}

impl Compile for ClassBody {
    fn compile(&self, state: &CompilationState) -> Result<Vec<CompiledItem>, anyhow::Error> {
        let mut result = vec![];

        for feature in &self.features {
            result.append(&mut feature.compile(state)?);
        }

        result.append(&mut self.constructor.compile(state)?);

        result.push(instruction!(ret));

        Ok(result)
    }
}

impl ClassBody {
    pub const fn new(features: Vec<ClassFeature>, constructor: Constructor) -> Self {
        Self {
            features,
            constructor,
        }
    }

    pub fn get_members(input: &Node) -> Result<Arc<[Ident]>> {
        let features = input.children();

        let mut fields = vec![];

        for member in features {
            let ty = ClassFeature::type_from_node(&member)?;
            fields.push(ty);
        }

        Ok(fields.into())
    }
}

impl Dependencies for ClassBody {
    fn supplies(&self) -> Vec<Dependency> {
        let mut features_sup: Vec<Dependency> =
            self.features.iter().flat_map(|x| x.supplies()).collect();

        features_sup.append(&mut self.constructor.supplies());

        features_sup
    }

    fn dependencies(&self) -> Vec<Dependency> {
        let mut block_dependencies: Vec<Dependency> = self
            .features
            .iter()
            .flat_map(|x| x.net_dependencies())
            .collect();

        block_dependencies.append(&mut self.constructor.net_dependencies());

        block_dependencies
    }

    fn net_dependencies(&self) -> Vec<Dependency> {
        get_net_dependencies(self, true)
    }
}

impl Parser {
    pub fn class_body(input: Node) -> Result<ClassBody, Vec<anyhow::Error>> {
        let class_features_node = input.children();

        let mut constructor: Option<Constructor> = None;

        let mut class_features = vec![];

        for feature_node in class_features_node {
            use ClassFeature as CF;
            match feature_node.as_rule() {
                Rule::class_constructor => {
                    if constructor.is_some() {
                        return Err(vec![new_err(
                            feature_node.as_span(),
                            &input.user_data().get_file_name(),
                            "Cannot have multiple constructors in a class".to_owned(),
                        )]);
                    }

                    constructor = Some(Self::constructor(feature_node)?);

                    // class_features.push(CF::Constructor());
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

        let constructor = constructor.unwrap_or_else(|| {
            let path_str = input.user_data().bytecode_path();
            let class_type = input
                .user_data()
                .get_type_of_executing_class_in_nth_frame(0)
                .unwrap();
            let class_name = class_type.arced_name();

            Constructor::default_constructor(path_str, class_name)
        });

        Ok(ClassBody::new(class_features, constructor))
    }
}
