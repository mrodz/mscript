use anyhow::Result;
use pest_consume::Parser as ParserDerive;

use crate::ast::{Compile, CompiledItem, Declaration, CompiledFunctionId};

#[allow(unused)]
pub type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(ParserDerive)]
#[grammar = "grammar.pest"]
pub struct Parser;

pub fn root_node_from_str(str: &str) -> Result<Node> {
    Ok(<Parser as pest_consume::Parser>::parse(Rule::file, str)?.single()?)
}

#[derive(Debug, Default)]
pub struct File {
    pub declarations: Vec<Declaration>,
}

impl File {
    fn add_declaration(&mut self, declaration: Declaration) {
        self.declarations.push(declaration)
    }
}

impl Compile for File {
    fn compile(&self) -> Vec<CompiledItem> {
        let statements: Vec<CompiledItem> =
            self.declarations.iter().flat_map(Compile::compile).collect();
		
		let mut functions = vec![];
		let mut global_scope_code = vec![];
		
		for statement in statements {
			match statement {
				CompiledItem::Function { .. } => {
					functions.push(statement);
				}
				CompiledItem::Instruction { .. } => {
					global_scope_code.push(statement)
				}
			}
		}

		functions.push(CompiledItem::Function { id: CompiledFunctionId::Custom("main".into()), content: global_scope_code });

		dbg!(&functions);

		todo!()

        // functions
    }
}

#[pest_consume::parser]
impl Parser {
    pub fn file(input: Node) -> Result<File> {
        let mut result = File::default();

        for child in input.children() {
            match child.as_rule() {
                Rule::declaration => {
                    let d = Self::declaration(child)?;
                    result.add_declaration(d);
                }
                Rule::EOI => (),
                _ => unreachable!("{child:?}"),
            }
        }

        Ok(result)
    }
}
