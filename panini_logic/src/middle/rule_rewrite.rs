use std::collections::{BTreeMap, BTreeSet};

use cfg::Cfg;
use cfg::Symbol;
use gearley::grammar::Grammar;

use input::FragmentId;
use middle::flatten_stmts::{Path, Position};
use middle::symbol_maps::OuterSymbol;
use middle::trace::Trace;
use middle::ir::SymMap;

pub struct RuleRewrite<'a> {
    trace: &'a mut Trace,
    pub rules: BTreeMap<Path, RuleValue>,
    pub new_symbols: BTreeSet<Path>,
}

pub struct RuleRewriteResult {
    pub rules: BTreeMap<Path, RuleValue>,
    pub new_symbols: BTreeSet<Path>,
}

pub struct LowerRuleRewriteResult {
    pub rules: Vec<Rule>,
    pub new_symbols: BTreeSet<Path>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct RuleValue {
    pub lhs: Sym,
    pub rhs: BTreeMap<usize, Sym>,
    pub sequence: Option<(u32, Option<u32>)>,
    pub traces: BTreeMap<Option<usize>, usize>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Rule {
    pub lhs: Symbol,
    pub rhs: Vec<Symbol>,
    pub sequence: Option<(u32, Option<u32>)>,
    pub traces: BTreeMap<Option<usize>, usize>,
}

#[derive(Clone, Debug)]
enum Elem {
    Lhs {
        lhs: Sym,
    },
    Sequence {
        lhs: Sym,
        min: u32,
        max: Option<u32>,
    },
    Initial,
    End,
}

impl RuleValue {
    pub fn new(lhs: Sym) -> Self {
        RuleValue {
            lhs,
            rhs: BTreeMap::new(),
            sequence: None,
            traces: BTreeMap::new(),
        }
    }

    pub fn new_with_sequence(lhs: Sym, min: u32, max: Option<u32>) -> Self {
        RuleValue {
            lhs,
            rhs: BTreeMap::new(),
            sequence: Some((min, max)),
            traces: BTreeMap::new(),
        }
    }

    pub fn rhs(&self) -> Vec<Sym> {
        self.rhs.values().cloned().collect()
    }
}

// impl RuleValue {
//     fn replace(&mut self, sym: Sym, value: RuleValue) {

//         if self.rhs.values().any(|&rhs_sym| rhs_sym == sym) {
//             self.rhs_to_vec();
//         }
//     }   
// }

impl<'a> RuleRewrite<'a> {
    pub fn new(trace: &mut Trace) -> RuleRewrite {
        RuleRewrite {
            trace,
            rules: BTreeMap::new(),
            new_symbols: BTreeSet::new(),
        }
    }

    pub fn rewrite(&mut self, paths: Vec<Path>) {
        for mut path in paths {
            path.eliminate_binds();
            self.process_path(path);
        }
        self.collect_traces();
        // for (key, value) in &rules {
        //     if let Some((min, max)) = value.sequence {
        //         cfg.sequence(key.lhs).inclusive(min, max).rhs(value.rhs[&0]);
        //     } else {
        //         let mut rhs = vec![];
        //         for (_, &sym) in &value.rhs {
        //             rhs.push(sym);
        //         }
        //         cfg.rule(key.lhs).rhs(&rhs[..]);
        //     }
        // }
    }

    fn process_path(&mut self, path: Path) {
        path.position.iter().fold((Path { position: vec![] }, Elem::Initial), |acc, &elem| {
            match (acc.0, acc.1, elem) {
                (mut path, Elem::Initial, Position::IdxWithFragment { idx, fragment }) => {
                    path.position.push(Position::IdxWithFragment { idx, fragment});
                    (path, Elem::Lhs { lhs: Sym::Fragment(fragment) })
                }
                (mut path, Elem::Lhs { lhs }, alternative @ Position::Alternative(..)) => {
                    path.position.push(alternative);
                    (path, Elem::Lhs { lhs })
                }
                (path, Elem::Lhs { lhs }, Position::IdxWithFragment { idx, fragment }) => {
                    let value = RuleValue::new(lhs);
                    let entry = self.rules.entry(path.clone()).or_insert(value);
                    entry.rhs.insert(idx, Sym::Fragment(fragment));
                    let mut traced_path = path.clone();
                    traced_path.position.push(Position::IdxWithFragment { idx, fragment });
                    let (left_trace, right_trace) = self.trace.traces_for_path(&traced_path);
                    entry.traces.insert(Some(idx), left_trace);
                    entry.traces.insert(Some(idx + 1), right_trace);
                    (path, Elem::End)
                }
                (mut path, Elem::Lhs { lhs }, Position::Idx(idx)) => {
                    let value = RuleValue::new(lhs);
                    let entry = self.rules.entry(path.clone()).or_insert(value);
                    path.position.push(Position::Idx(idx));
                    entry.rhs.insert(idx, Sym::FromPath(path.clone()));
                    self.new_symbols.insert(path.clone());
                    (path.clone(), Elem::Lhs { lhs: Sym::FromPath(path) })
                }
                (mut path_prefix, Elem::Lhs { lhs }, Position::Sequence { min, max }) => {
                    let current_pos = Position::Sequence { min, max };
                    path_prefix.position.push(current_pos);
                    let mut value = RuleValue::new_with_sequence(lhs.clone(), min, max);
                    let (left_trace, right_trace) = self.trace.traces_for_path(&path_prefix);
                    value.traces.insert(Some(0), left_trace);
                    value.traces.insert(None, right_trace);
                    let new_lhs = if let Some(from_path) = path.new_rule(&path_prefix) {
                        value.rhs.insert(0, Sym::FromPath(from_path.clone()));
                        self.new_symbols.insert(from_path.clone());
                        Sym::FromPath(from_path)
                    } else {
                        lhs
                    };
                    self.rules.entry(path_prefix.clone()).or_insert(value);
                    (path_prefix, Elem::Lhs { lhs: new_lhs })
                }
                // (mut path, elem, Position::Bind(bind_id)) => {
                //     path.position.push(Position::Bind(bind_id));
                //     (path, elem)
                // }
                (path, elem, pos) => panic!("unexpected {:#?}", (path, elem, pos)),
            }
        });
    }

    fn collect_traces(&mut self) {
        for (_, rule_value) in &self.rules {
            self.trace.rule_tokens.push(
                rule_value.traces.clone()
            );
            match rule_value {
                &RuleValue { ref rhs, sequence: Some(..), .. } => {
                    if rhs.len() > 1 {
                        self.trace.rule_tokens.push(BTreeMap::new());
                    }
                }
                _ => {}
            }
        }
    }

    pub fn result(self) -> RuleRewriteResult {
        RuleRewriteResult {
            rules: self.rules,
            new_symbols: self.new_symbols,
        }
    }

    // fn simplify(&mut self) {
    //     let mut map = btreemap!{};
    //     for key in self.rules.keys() {
    //         let entry = map.entry(key.lhs).or_insert((0, 0));
    //         entry.0 += 1;
    //     }
    //     for value in self.rules.values() {
    //         let multiple_rhs = value.rhs().len() > 1;
    //         for (&pos, &sym) in &value.rhs {
    //             let entry = map.entry(sym).or_insert((0, 0));
    //             entry.1 += 1;
    //         }
    //     }
    //     for (sym, occurences) in map {
    //         if occurences == (1, 1, true) {
    //             let rule = self.rules.iter().find(|key, value| key.lhs == sym).map(|key, value| value.clone()).unwrap();
    //             for value in self.rules.values_mut() {
    //                 let rhs = value.replace(sym, rule);
    //             }
    //         }
    //     }
    // }
}

impl RuleRewriteResult {
    pub fn lower(self, grammar: &mut Grammar, symbol_maps: &mut SymbolMaps) -> LowerRuleRewriteResult {
        let mut result = LowerRuleRewriteResult {
            rules: vec![],
            new_symbols: self.new_symbols,
        };
        for (_, rule_value) in self.rules {
            match rule_value {
                RuleValue { lhs, rhs, sequence: Some((min, max)), traces } => {
                    if rhs.len() == 1 {
                        let lhs = symbols_map.intern(grammar, &lhs);
                        let rhs = rhs.get(&0).unwrap();
                        let rhs = symbols_map.intern(grammar, rhs);
                        result.rules.push(Rule { lhs, rhs: vec![rhs], sequence: Some((min, max)), traces });
                    } else {
                        let intermediate: Symbol = grammar.sym();
                        let lhs = symbols_map.intern(grammar, &lhs);
                        let rhs = rhs.values().map(|sym| symbols_map.intern(grammar, sym)).collect();
                        result.rules.push(Rule { lhs: intermediate, rhs, sequence: None, traces });
                        result.rules.push(Rule {
                            lhs,
                            rhs: vec![intermediate],
                            sequence: Some((min, max)),
                            traces: BTreeMap::new(),
                        });
                    }
                }
                RuleValue { lhs, rhs, sequence: None, traces } => {
                    let lhs = symbols_map.intern(grammar, &lhs);
                    let rhs = rhs.values().map(|sym| symbols_map.intern(grammar, sym)).collect();
                    result.rules.push(Rule { lhs, rhs, sequence: None, traces });
                }
            }
        }
        result
    }
}

// impl RuleValue {
//     fn lower(&self, grammar: &mut Grammar, sym_map)
// }
