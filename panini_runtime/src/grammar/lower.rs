use std::collections::BTreeMap;
use std::rc::Rc;

use panini_logic::input::{FragmentId, ExprId};
use panini_logic::input::ast::{Stmts, Stmt, Alternative, Action, Rhs, RhsAst, RhsElement, Sequence};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::output::instruction::InstructionList;

use crate::enum_stream::EnumStreamGrammar;
use crate::grammar::{Grammar, Rule};
use crate::grammar::parser::{Parser, Tables};

impl<N, T> Grammar<N, T> {
    pub fn lower(self) -> Parser<N, T> where T: Copy {
        let mut parser = Parser::new(self.sub);
        let mut stmts = vec![];
        for (lhs, rule) in self.rules {
            let lhs = parser.tables.intern_fragment(lhs);
            let body = parser.tables.lower_sum(rule);
            stmts.push(Stmt { lhs, body, ty: None });
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
    fn lower_sum(&mut self, rule: Rule<N>) -> Vec<Alternative> {
        match rule {
            Rule::Sum(summands) => {
                summands.into_iter().map(|summand| {
                    let expr = Some(self.intern_action(summand.action.clone()));
                    (0, self.lower_rhs(summand.rule), Action { expr })
                }).collect()
            }
            _ => unreachable!()
        }
    }

    fn lower_rhs(&mut self, rule: Rule<N>) -> Rhs {
        match rule {
            Rule::Sum(..) => unreachable!(),
            Rule::Bound(rule, bound) => {
                Rhs(vec![
                    RhsElement {
                        bind: Some(0),
                        elem: self.lower_rhs_ast(*rule),
                    }
                ])
            }
            Rule::Call(callee) => {
                Rhs(vec![
                    RhsElement {
                        bind: None,
                        elem: RhsAst::Fragment(self.intern_fragment(callee)),
                    }
                ])
            }
            Rule::Repeat(rule) => {
                Rhs(vec![
                    RhsElement {
                        bind: None,
                        elem: RhsAst::Sequence(Sequence {
                            rhs: self.lower_rhs(*rule),
                            min: 0,
                            max: None,
                        })
                    }
                ])
            }
            Rule::Product(factors) => {
                Rhs(factors.into_iter().map(|factor|
                    RhsElement {
                        bind: None,
                        elem: self.lower_rhs_ast(factor),
                    }
                ).collect())
            }
        }
    }

    fn lower_rhs_ast(&mut self, rule: Rule<N>) -> RhsAst {
        match rule {
            Rule::Sum(..) => unreachable!(),
            Rule::Bound(rule, bound) => {
                RhsAst::Product(
                    Rhs(vec![
                        RhsElement {
                            bind: Some(0),
                            elem: self.lower_rhs_ast(*rule),
                        }
                    ])
                )
            }
            Rule::Call(callee) => {
                RhsAst::Fragment(self.intern_fragment(callee))
            }
            Rule::Repeat(rule) => {
                RhsAst::Sequence(Sequence {
                    rhs: self.lower_rhs(*rule),
                    min: 0,
                    max: None,
                })
            }
            Rule::Product(factors) => {
                RhsAst::Product(
                    Rhs(factors.into_iter().map(|factor|
                        RhsElement {
                            bind: None,
                            elem: self.lower_rhs_ast(factor),
                        }
                    ).collect())
                )
            }
        }
    }
}
