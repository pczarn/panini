use std::hash::Hash;

use cfg::{Symbol, ContextFree};
use gearley::grammar::Grammar;

use middle::{Action, ActionExpr};
use middle::trace::SourceOrigin;
use input::SpanId;

use self::RuleProperties::*;

#[derive(Clone)]
pub(super) struct Rule {
    lhs: Symbol,
    properties: RuleProperties,
    action: ActionExpr,
    source_origin: SourceOrigin,
}

#[derive(Clone, Debug)]
pub struct PrecedencedRuleAlternative {
    pub(super) level: u32,
    pub(super) rhs_and_binds: RhsAndBinds,
    pub(super) action: ActionExpr,
    pub(super) source_origin: SourceOrigin,
}

pub enum RuleProperties<S> {
    SimpleRuleProperties(RhsAndBinds),
    SequenceProperties {
        rhs: S,
        span: SpanId,
        min: u32,
        max: Option<u32>,
    },
    PrecedencedRuleProperties {
        rhs_levels: PrecedencedRuleAlternatives<S>,
    },
}

pub struct RhsAndBinds {
    pub(super) rhs: Vec<Symbol>,
    pub(super) tuple_binds: Vec<usize>,
    pub(super) deep_binds: Vec<usize>,
    pub(super) shallow_binds: Vec<(usize, rs::Ident)>,
}

pub struct PrecedencedRuleAlternatives(Vec<PrecedencedRuleAlternative>);

impl PrecedencedRuleAlternatives {
    fn by_level(&self) -> HashMap<u32, Vec<PrecedencedRuleAlternative<S>>> {
        let mut map = HashMap::new();
        for alternative in &self.0 {
            let entry = map.entry(alternative.level).or_insert(vec![]);
            entry.push(alternative.clone());
        }
        map
    }
}

/// A basic rule has simplified information about a rule.
/// how to say the action is important, the action is carried
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicRule<S = Symbol> {
    pub lhs: S,
    pub lhs_span: SpanId,
    pub rhs: Vec<S>,
    pub action: Action,
}

impl Rule {
    pub fn add_to(&self, grammar: &mut Grammar) {
        match &self.properties {
            &BnfRuleProperties(ref rhs_and_binds) => {
                let rhs_syms: Vec<_>;
                rhs_syms = rhs.iter().map(|s| s).collect();
                grammar.rule(self.lhs.node).rhs(&rhs_syms);
            }
            &SequenceProperties { rhs, min, max, .. } => {
                grammar.sequence(self.lhs.node).inclusive(min, max).rhs(rhs.node);
            }
            &PrecedencedRuleProperties { ref rhs_levels, .. } => {
                let mut rule = grammar.precedenced_rule(self.lhs.node);
                for (level, alternatives) in rhs_levels.by_level() {
                    for rule in &alternatives.0 {
                        let rhs_syms: Vec<_>;
                        rhs_syms = rule.rhs_and_binds.rhs.iter().map(|s| s.node).collect();
                        rule = rule.rhs(&rhs_syms);
                    }
                    rule = rule.lower_precedence();
                }
            }
        }
    }

    pub fn basic_rules(&self) -> Vec<BasicRule> {
        fn make_action(action_expr: &ActionExpr, rhs_and_binds: &RhsAndBinds) -> Action {
            let action;
            if action_expr.is_inline() ||
                    !rhs_and_binds.deep_binds.is_empty() ||
                    !rhs_and_binds.shallow_binds.is_empty() {
                action = Action::Struct {
                    deep_binds: rhs_and_binds.deep_binds.clone(),
                    shallow_binds: rhs_and_binds.shallow_binds.clone(),
                    expr: action_expr.clone(),
                };
            } else {
                action = Action::Tuple {
                    tuple_binds: rhs_and_binds.tuple_binds.clone(),
                };
            }
            action
        }

        match &self.properties {
            &BnfRuleProperties(ref rhs_and_binds) => {
                let basic_rule = BasicRule {
                    lhs: lhs,
                    rhs: rhs_and_binds.rhs.clone(),
                    action: make_action(&self.action, rhs_and_binds),
                };
                vec![basic_rule]
            }
            &SequenceProperties { rhs, .. } => {
                let basic_rule = BasicRule {
                    lhs: self.lhs,
                    rhs: vec![rhs],
                    action: Action::Sequence,
                };
                vec![basic_rule]
            }
            &PrecedencedRuleProperties { ref rhs_levels, .. } => {
                let mut basic_rules = vec![];
                for alternative in &rhs_levels.0 {
                    let basic_rule = BasicRule {
                        // where from?
                        lhs: lhs,
                        lhs: self.lhs,
                        rhs: alternative.rhs.clone(),
                        action: make_action(&alternative.action, &alternative.rhs_and_binds),
                    };
                    basic_rules.push(basic_rule);
                }
                basic_rules
            }
        }
    }

    pub fn source_origins(&self) -> Vec<SourceOrigin> {
        match self {
            &Rule::BnfRule { ref source_origin, .. } => {
                vec![source_origin.clone()]
            }
            &Rule::SequenceRule { ref source_origin, .. } => {
                vec![source_origin.clone()]
            }
            &Rule::PrecedencedRule { ref rhs_levels, .. } => {
                rhs_levels.iter().flat_map(|level| {
                    level.rules.iter().map(|rule| {
                        rule.source_origin.clone()
                    })
                }).collect()
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
            } => {
                Rule::PrecedencedRule {
                    lhs: self.fold_spanned_symbol(lhs),
                    rhs_levels: rhs_levels.into_iter().map(|precedence_level| {
                        let rules = precedence_level.rules.into_iter().map(|alt| {
                            PrecedencedRuleAlternative {
                                rhs: self.fold_spanned_symbols(alt.rhs),
                                tuple_binds: alt.tuple_binds,
                                shallow_binds: alt.shallow_binds,
                                deep_binds: alt.deep_binds,
                                action: alt.action,
                                source_origin: alt.source_origin,
                            }
                        }).collect();
                        PrecedenceLevel {
                            rules: rules
                        }
                    }).collect(),
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
