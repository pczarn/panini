use std::hash::Hash;

use cfg::Symbol;
use gearley::grammar::Grammar;

use rs;
use middle::{Action, ActionExpr};
use middle::trace::SourceOrigin;

#[derive(Clone)]
pub enum Rule<S = Symbol> {
    BnfRule {
        lhs: rs::Spanned<S>,
        rhs: Vec<rs::Spanned<S>>,
        tuple_binds: Vec<usize>,
        deep_binds: Vec<usize>,
        shallow_binds: Vec<(usize, rs::ast::Ident)>,
        action: ActionExpr,
        source_origin: SourceOrigin,
    },
    SequenceRule {
        lhs: rs::Spanned<S>,
        rhs: rs::Spanned<S>,
        min: u32,
        max: Option<u32>,
        action: ActionExpr,
        source_origin: SourceOrigin,
    },
    PrecedencedRule {
        lhs: rs::Spanned<S>,
        rhs_levels: Vec<PrecedenceLevel<S>>,
        action: ActionExpr,
        source_origin: SourceOrigin,
    },
}

#[derive(Clone)]
pub struct PrecedenceLevel<S> {
    rules: Vec<PrecedencedRuleAlternative<S>>,
}

#[derive(Clone)]
pub struct PrecedencedRuleAlternative<S> {
    rhs: Vec<rs::Spanned<S>>,
    action: ActionExpr,
}

/// A basic rule has simplified information about a rule.
/// how to say the action is important, the action is carried
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicRule<S = Symbol> {
    pub lhs: rs::Spanned<S>,
    pub rhs: Vec<rs::Spanned<S>>,
    pub action: Action,
}

impl Rule {
    pub fn add_to(&self, grammar: &mut Grammar) {
        match self {
            &Rule::BnfRule { lhs, ref rhs, .. } => {
                let rhs_syms: Vec<_>;
                rhs_syms = rhs.iter().map(|s| s.node).collect();
                grammar.rule(lhs.node).rhs(&rhs_syms);
            }
            &Rule::SequenceRule { lhs, rhs, min, max, .. } => {
                grammar.sequence(lhs.node).inclusive(min, max).rhs(rhs.node);
            }
            &Rule::PrecedencedRule { lhs, ref rhs_levels, .. } => {
                let mut rule = grammar.precedenced_rule(lhs.node);
                for rhs_level in rhs_levels {
                    for rhs in &rhs_level.rules {
                        let rhs_syms: Vec<_>;
                        rhs_syms = rhs.rhs.iter().map(|s| s.node).collect();
                        rule = rule.rhs(&rhs_syms);
                    }
                    rule = rule.lower_precedence();
                }
            }
        }
    }

    pub fn basic_rules(&self) -> Vec<BasicRule> {
        match self {
            &Rule::BnfRule { lhs, ref rhs, action: ref action_expr, ref deep_binds, ref shallow_binds, ref tuple_binds, .. } => {
                let action;
                if action_expr.is_inline() ||
                        !deep_binds.is_empty() ||
                        !shallow_binds.is_empty() {
                    action = Action::Struct {
                        deep_binds: deep_binds.clone(),
                        shallow_binds: shallow_binds.clone(),
                        expr: action_expr.clone(),
                    };
                } else {
                    action = Action::Tuple {
                        tuple_binds: tuple_binds.clone(),
                    };
                }
                let basic_rule = BasicRule {
                    lhs: lhs,
                    rhs: rhs.clone(),
                    action: action,
                };
                vec![basic_rule]
            }
            &Rule::SequenceRule { lhs, rhs, .. } => {
                let basic_rule = BasicRule {
                    lhs: lhs,
                    rhs: vec![rhs],
                    action: Action::Sequence,
                };
                vec![basic_rule]
            }
            &Rule::PrecedencedRule { ref rhs_levels, .. } => {
                let mut basic_rules = vec![];
                // for level in rhs_levels {
                //     for rule in &level.rules {
                //         let basic_rule = BasicRule {
                //             // where from?
                //             lhs: rule.lhs,
                //             rhs: vec![rule.rhs],
                //             action: Action::Sequence,
                //         };
                //         basic_rules.push(basic_rule);
                //     }
                // }
                basic_rules
            }
        }
    }

    pub fn source_origin(&self) -> &SourceOrigin {
        match self {
            &Rule::BnfRule { ref source_origin, .. } => {
                source_origin
            }
            &Rule::SequenceRule { ref source_origin, .. } => {
                source_origin
            }
            &Rule::PrecedencedRule { ref source_origin, .. } => {
                source_origin
            }
        }
    }
}

pub trait FoldRule<S1>
    where S1: Eq + Hash
{
    type Symbol: Eq + Hash;

    fn fold_rule(&mut self, rule: Rule<S1>) -> Rule<Self::Symbol> {
        match rule {
            Rule::BnfRule {
                lhs,
                rhs,
                tuple_binds,
                deep_binds,
                shallow_binds,
                action,
                source_origin,
            } => {
                Rule::BnfRule {
                    lhs: self.fold_spanned_symbol(lhs),
                    rhs: self.fold_spanned_symbols(rhs),
                    tuple_binds: tuple_binds,
                    deep_binds: deep_binds,
                    shallow_binds: shallow_binds,
                    action: action,
                    source_origin: source_origin,
                }
            }
            Rule::SequenceRule {
                lhs,
                rhs,
                min,
                max,
                action,
                source_origin,
            } => {
                Rule::SequenceRule {
                    lhs: self.fold_spanned_symbol(lhs),
                    rhs: self.fold_spanned_symbol(rhs),
                    min: min,
                    max: max,
                    action: action,
                    source_origin: source_origin,
                }
            }
            Rule::PrecedencedRule {
                lhs,
                rhs_levels,
                action,
                source_origin,
            } => {
                Rule::PrecedencedRule {
                    lhs: self.fold_spanned_symbol(lhs),
                    rhs_levels: rhs_levels.into_iter().map(|precedence_level| {
                        let rules = precedence_level.rules.into_iter().map(|alt| {
                            PrecedencedRuleAlternative {
                                rhs: self.fold_spanned_symbols(alt.rhs),
                                action: alt.action,
                            }
                        }).collect();
                        PrecedenceLevel {
                            rules: rules
                        }
                    }).collect(),
                    action: action,
                    source_origin: source_origin,
                }
            }
        }
    }

    fn fold_spanned_symbols(&mut self, list: Vec<rs::Spanned<S1>>) -> Vec<rs::Spanned<Self::Symbol>> {
        list.into_iter().map(|sym| self.fold_spanned_symbol(sym)).collect()
    }

    fn fold_spanned_symbol(&mut self, symbol: rs::Spanned<S1>) -> rs::Spanned<Self::Symbol> {
        rs::respan(symbol.span, self.fold_symbol(symbol.node))
    }

    fn fold_symbol(&mut self, symbol: S1) -> Self::Symbol;
}
