use std::collections::BTreeMap;

use cfg::Cfg;
use cfg::Symbol;

use input::FragmentId;
use middle::flatten_stmts::{Path, Position};
use middle::trace::Trace;

pub struct RuleRewrite<'a> {
    cfg: Cfg,
    trace: &'a Trace,
    pub rules: BTreeMap<RuleKey, RuleValue>,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct RuleKey {
    pub lhs: Sym,
    pub path: Path,
}

#[derive(Debug, Eq, PartialEq)]
pub struct RuleValue {
    pub rhs: BTreeMap<usize, Sym>,
    pub sequence: Option<(u32, Option<u32>)>,
    pub traces: BTreeMap<Option<usize>, usize>,
}

enum Elem {
    Stmt {
        lhs: Sym,
    },
    Rule {
        lhs: Sym,
    },
    Initial,
    End,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Sym {
    Fragment(FragmentId),
    Symbol(Symbol),
}

impl RuleValue {
    pub fn new() -> Self {
        RuleValue {
            rhs: BTreeMap::new(),
            sequence: None,
            traces: BTreeMap::new(),
        }
    }

    pub fn new_with_sequence(min: u32, max: Option<u32>) -> Self {
        RuleValue {
            rhs: BTreeMap::new(),
            sequence: Some((min, max)),
            traces: BTreeMap::new(),
        }
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
    pub fn new(cfg: Cfg, trace: &Trace) -> RuleRewrite {
        RuleRewrite {
            cfg,
            trace,
            rules: BTreeMap::new(),
        }
    }

    pub fn rewrite(&mut self, paths: Vec<Path>) {
        for path in paths {
            path.position.iter().fold((Path { position: vec![] }, Elem::Initial), |acc, &elem| {
                match (acc, elem) {
                    ((mut path, Elem::Initial), Position::IdxWithFragment { idx, fragment }) => {
                        path.position.push(Position::IdxWithFragment { idx, fragment});
                        (path, Elem::Stmt { lhs: Sym::Fragment(fragment) })
                    }
                    ((mut path, Elem::Stmt { lhs }), alternative @ Position::Alternative(..)) => {
                        path.position.push(alternative);
                        (path, Elem::Rule { lhs })
                    }
                    ((path, Elem::Rule { lhs }), Position::IdxWithFragment { idx, fragment }) => {
                        let key = RuleKey { lhs, path: path.clone() };
                        let value = RuleValue::new();
                        let entry = self.rules.entry(key).or_insert(value);
                        entry.rhs.insert(idx, Sym::Fragment(fragment));
                        let mut traced_path = path.clone();
                        traced_path.position.push(Position::IdxWithFragment { idx, fragment });
                        let (left_trace, right_trace) = self.trace.traces_for_path(&traced_path);
                        entry.traces.insert(Some(idx), left_trace);
                        entry.traces.insert(Some(idx + 1), right_trace);
                        (path, Elem::End)
                    }
                    ((mut path, Elem::Rule { lhs }), Position::Idx(idx)) => {
                        let sym: Symbol = self.cfg.sym();
                        let key = RuleKey { lhs, path: path.clone() };
                        let value = RuleValue::new();
                        let entry = self.rules.entry(key).or_insert(value);
                        entry.rhs.insert(idx, Sym::Symbol(sym));
                        path.position.push(Position::Idx(idx));
                        (path, Elem::Stmt { lhs: Sym::Symbol(sym) })
                    }
                    ((mut path, Elem::Rule { lhs }), Position::Sequence { min, max }) |
                    ((mut path, Elem::Stmt { lhs }), Position::Sequence { min, max }) => {
                        path.position.push(Position::Sequence { min, max });
                        let key = RuleKey { lhs, path: path.clone() };
                        let mut value = RuleValue::new_with_sequence(min, max);
                        let (left_trace, right_trace) = self.trace.traces_for_path(&path);
                        value.traces.insert(Some(0), left_trace);
                        value.traces.insert(None, right_trace);
                        self.rules.entry(key).or_insert(value);
                        (path, Elem::Rule { lhs })
                    }
                    _ => unreachable!()
                }
            });
        }
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
