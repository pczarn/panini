use std::collections::BTreeMap;
use std::rc::Rc;

use panini_logic::input::ast::{
    InputTree, Pathway, Step,
};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::input::{ExprId, FragmentId};
use panini_logic::output::instruction::InstructionList;

use crate::enum_stream::EnumStreamGrammar;
use crate::grammar::parser::{Parser, Tables};
use crate::grammar::{Grammar, Rule};

impl<N, T> Grammar<N, T> {
    pub fn lower(self) -> Parser<N, T>
    where
        T: Copy,
    {
        let mut parser = Parser::new(self.sub);
        let mut stmts = vec![];
        for (lhs, rule) in self.rules {
            let lhs = parser.tables.intern_fragment(lhs);
            let body = parser.tables.lower_sum(rule);
            stmts.push(Stmt {
                lhs,
                body,
                ty: None,
            });
        }
        let stmts = Stmts {
            attr_arguments: AttrArguments {
                lexer_arguments: None,
            },
            stmts,
            lexer: Some(0),
        };
        parser.process(stmts);
        parser
    }
}

impl<N> Tables<N> {
    fn lower_sum(&mut self, rule: Rule<N>) -> Pathway {
        match rule {
            Rule::Sum(summands) => {
                Pathway {
                    steps: vec![]
                    tail: vec![]
                }
                summands
                .into_iter()
                .map(|summand| {
                    let expr = Some(self.intern_action(summand.action.clone()));
                    (0, self.lower_rhs(summand.rule), Action { expr })
                })
                .collect()
            }
            _ => unreachable!(),
        }
    }

    fn lower_rhs(&mut self, rule: Rule<N>) -> Pathway {
        match rule {
            Rule::Sum(..) => unreachable!(),
            Rule::Bound(rule, bound) => {
                self.lower_rhs().prepend(Step::Bind { bind_id: 0, idx: 0 })
            }
            Rule::Call(callee) => {
                let step = Step::Fragment(self.intern_fragment(callee));
                Pathway {
                    steps: vec![step],
                    tail: vec![],
                }
            }
            Rule::Repeat(rule) => {
                let (min, max) = (0, None)
                self.lower_rhs(*rule).prepend(Step::Sequence { min, max })
            }]),
            Rule::Product(factors) => {
                let lower_factor = |factor| self.lower_rhs(factor).simplify();
                Pathway {
                    steps: vec![],
                    tail: factors.into_iter().map(lower_factor).collect(),
                }
            }
        }
    }

    fn lower_rhs_ast(&mut self, rule: Rule<N>) -> RhsAst {
        match rule {
            Rule::Sum(..) => unreachable!(),
            Rule::Bound(rule, bound) => RhsAst::Product(Rhs(vec![RhsElement {
                bind: Some(0),
                elem: self.lower_rhs_ast(*rule),
            }])),
            Rule::Call(callee) => RhsAst::Fragment(self.intern_fragment(callee)),
            Rule::Repeat(rule) => RhsAst::Sequence(Sequence {
                rhs: self.lower_rhs(*rule),
                min: 0,
                max: None,
            }),
            Rule::Product(factors) => RhsAst::Product(Rhs(factors
                .into_iter()
                .map(|factor| RhsElement {
                    bind: None,
                    elem: self.lower_rhs_ast(factor),
                })
                .collect())),
        }
    }
}
