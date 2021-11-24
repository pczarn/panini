use std::collections::BTreeMap;
use std::rc::Rc;

use panini_logic::input::ast::{
    InputTree, PathwayGraph, Step,
};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::input::{ExprId, FragmentId};
use panini_logic::output::instruction::InstructionList;

use crate::enum_stream::EnumStreamGrammar;
use crate::grammar::parser::{Parser, Tables};
use crate::grammar::{Grammar, Rule, Summand};

struct Lower<N> {
    tables: Tables<N>,
    pathway_graph: PathwayGraph,
}

impl<N, T> Grammar<N, T> {
    pub fn make_lower(rules: BTreeMap<String, Rule<N>>) -> Lower<N> {
        let mut lower = Lower::new();
        for (lhs, rule) in self.rules.into_iter() {
            lower.lower_rhs(Some(lhs), rule);
        }
        lower
    }

    pub fn lower(self) -> Parser<N, T>
    where
        T: Copy,
    {
        let mut parser = Parser::new(self.sub);
        let lower = Grammar::make_lower(self.rules);
        parser.tables = lower.tables;
        let stmts = InputTree {
            attr_arguments: AttrArguments {
                lexer_arguments: None,
            },
            pathway_graph: lower.pathway_graph,
            lexer: Some(0),
        };
        parser.process(stmts);
        parser
    }
}

impl<N> Lower<N> {
    fn new() -> Self {
        Lower {
            tables: Tables::new(),
            pathway_graph: PathwayGraph::new(),
        }
    }

    fn lower_rhs(&mut self, lhs: Option<String>, rule: Rule<N>) -> usize {
        assert!(lhs.none() ^ rule.is_sum());
        match rule {
            Rule::Sum(summands) => {
                self.pathway_graph.step_node(
                    Step::StmtFragment(lhs),
                    self.pathway_graph.branch_node(
                        self.lower_summands(lhs.unwrap(), summands)
                    ),
                )
            }
            Rule::Bound(inner_rule, bound) => {
                let (bind_id, idx) = (0, 0);
                self.pathway_graph.step_node(
                    Step::Bind { bind_id, idx },
                    self.lower_rhs(None, *inner_rule),
                )
            }
            Rule::Call(callee) => {
                self.pathway_graph.step_node(
                    Step::Fragment(self.intern_fragment(callee)),
                    NULL_NODE,
                )
            }
            Rule::Repeat(inner_rule) => {
                let (min, max) = (0, None);
                self.pathway_graph.step_node(
                    Step::Sequence { min, max },
                    self.lower_rhs(None, *inner_rule),
                )
            }]),
            Rule::Product(factors) => {
                self.pathway_graph.branch_node(
                    factors.into_iter().map(|factor| self.lower_rhs(None, factor))
                )
            }
        }
    }

    fn lower_summands(&mut self, summands: Vec<Summand<N>>) -> Vec<usize> {
        for (i, summand) in summands.iter().enumerate() {
            self.stmt_map.insert((lhs, i), self.intern_action(summand.action));
        }
        summands.into_iter().enumerate().map(|(i, summand)|
            self.pathway_graph.step_node(
                Step::StmtIdx(i),
                self.lower_rhs(summand.rule),
            )
        ).collect()
    }
}
