use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::cmp::{Ord, PartialOrd, Ordering};
use std::ops::Range;

use input::{BindId, FragmentId};
use input::ast::{Stmts, RhsAst, Rhs};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Path {
    pub position: Vec<Position>,
}

// impl Ord for Path {
//     fn cmp(&self, other: &Self) -> Ordering {
//         let a: Vec<_> = self.position.iter().filter(|pos| !pos.is_bind()).cloned().collect();
//         let b: Vec<_> = other.position.iter().filter(|pos| !pos.is_bind()).cloned().collect();
//         a.cmp(&b)
//     }
// }

// impl PartialOrd for Path {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         Some(self.cmp(other))
//     }
// }

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Position {
    Alternative(usize),
    Idx(usize),
    IdxWithFragment {
        idx: usize,
        fragment: FragmentId,
    },
    StmtFragment(FragmentId),
    StmtIdx(usize),
    // Class(Class, Symbol),
    // RightQuote,
    Bind(BindId),
    Sequence {
        min: u32,
        max: Option<u32>,
    },
    SequenceEnd,
    SequenceToken,
    Max,
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> Ordering {
        let a = self.canonicalize();
        let b = other.canonicalize();
        a.cmp(&b)
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct FlattenStmts {
    pub paths: Vec<Path>,
}

impl Path {
    pub fn split_last(&self) -> (&Position, Path) {
        let (last, prefix) = self.position.split_last().unwrap();
        let prefix_path = Path { position: prefix.to_vec() };
        (last, prefix_path)
    }

    pub fn eliminate_binds(&mut self) {
        self.position.retain(|elem| !elem.is_bind());
    }

    pub fn new_rule(&self, prefix: &Path) -> Option<Path> {
        if self.position[prefix.position.len() ..].iter().any(|pos| pos.is_sequence()) {
            let from_path = prefix.clone();
            Some(from_path)
        } else {
            None
        }
    }

    fn canonicalize(&self) -> Cow<Vec<Position>> {
        match (self.position.get(0), self.position.get(1)) {
            (Some(&Position::StmtFragment(fragment)), Some(&Position::StmtIdx(idx))) => {
                let mut position = self.position.clone();
                position[0] = Position::IdxWithFragment { idx, fragment };
                position.remove(1);
                Cow::Owned(position)
            }
            // (Some(&Position::StmtFragment(fragment)), None) => {
            //     Cow::Owned(vec![Position::IdxWithFragment { fragment, idx: 0 }])
            // }
            _ => Cow::Borrowed(&self.position)
        }
    }

    fn split_stmt(mut self) -> Path {
        match self.position[0] {
            Position::IdxWithFragment { idx, fragment } => {
                self.position[0] = Position::StmtFragment(fragment);
                self.position.insert(1, Position::StmtIdx(idx));
            }
            _ => {}
        }
        self
    }

    pub fn range(self) -> Range<Path> {
        match (self.position.get(0), self.position.get(1)) {
            (Some(&Position::StmtFragment(fragment)), None) => {
                let start = Path {
                    position: vec![
                        Position::IdxWithFragment { idx: 0, fragment },
                    ]
                };
                let end = Path {
                    position: vec![
                        Position::IdxWithFragment { idx: !0, fragment },
                        Position::Max,
                    ]
                };
                start .. end
            }
            _ => {
                let mut end = self.clone();
                end.position.push(Position::Max);
                self .. end
            }
        }
    }
}

impl Position {
    fn is_bind(&self) -> bool {
        match self {
            &Position::Bind(..) => true,
            _ => false,
        }
    }

    fn is_sequence(&self) -> bool {
        match self {
            &Position::Sequence { .. } => true,
            _ => false,
        }
    }
}

impl FlattenStmts {
    pub fn new() -> Self {
        FlattenStmts {
            paths: vec![],
        }
    }

    pub fn flatten_stmts(&mut self, stmts: &Stmts) {
        for (stmt_idx, stmt) in stmts.stmts.iter().enumerate() {
            for (alternative_idx, (_level, ref rhs, ref _action)) in stmt.body.iter().enumerate() {
                let mut path = Path {
                    position: vec![
                        Position::IdxWithFragment {
                            idx: stmt_idx,
                            fragment: stmt.lhs,
                        },
                    ],
                };
                if stmt.body.len() > 1 {
                    path.position.push(Position::Alternative(alternative_idx));
                }
                self.flatten_rhs(path, rhs);
            }
        }
    }

    fn flatten_rhs(&mut self, path: Path, rhs: &Rhs) {
        for (rhs_idx, element) in rhs.0.iter().enumerate() {
            let mut path = path.clone();
            if let Some(bind_id) = element.bind {
                path.position.push(Position::Bind(bind_id));
            }
            match &element.elem {
                &RhsAst::Fragment(_) => {}
                _ => {
                    if rhs.0.len() > 1 && element.bind.is_none() {
                        path.position.push(Position::Idx(rhs_idx));
                    }
                }
            }
            match &element.elem {
                &RhsAst::Fragment(fragment_id) => {
                    path.position.push(Position::IdxWithFragment {
                        idx: rhs_idx,
                        fragment: fragment_id,
                    });
                    self.paths.push(path);
                }
                &RhsAst::String(ref _string) => {
                    // path.position.push(Position::IdxWithString {
                    //     idx: rhs_idx,
                    //     string,
                    // });
                    // self.paths.push(path);
                    unimplemented!()
                }
                &RhsAst::Sequence(ref sequence) => {
                    path.position.push(Position::Sequence {
                        min: sequence.min,
                        max: sequence.max,
                    });
                    self.flatten_rhs(path, &sequence.rhs);
                }
                &RhsAst::Sum(ref summands) => {
                    if summands.is_empty() {
                        self.paths.push(path.clone());
                    }
                    let summands_len = summands.len();
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let mut path = path.clone();
                        if summands_len > 1 {
                            path.position.push(Position::Alternative(summand_idx));
                        }
                        self.flatten_rhs(path, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
                    self.flatten_rhs(path, rhs);
                }
            }
        }
        if rhs.0.is_empty() {
            self.paths.push(path);
        }
    }

    pub fn join_stmts(&self) -> Vec<Path> {
        // let mut rhs_count: BTreeMap<FragmentId, BTreeSet<(usize, usize)>> = BTreeMap::new();
        // for path in &self.paths {
        //     match (path.position[0], path.position[1]) {
        //         (Position::IdxWithFragment { idx, fragment }, Position::Alternative(alt)) => {
        //             let value = rhs_count.entry(fragment).or_insert(BTreeSet::new());
        //             value.insert((idx, alt));
        //         }
        //         (Position::IdxWithFragment { idx, fragment }, _) => {
        //             let value = rhs_count.entry(fragment).or_insert(BTreeSet::new());
        //             value.insert((idx, 0));
        //         }
        //         _ => unreachable!()
        //     }
        // }
        // self.paths.clone().into_iter().map(|mut path| {
        //     match (path.position[0], path.position[1]) {
        //         (Position::IdxWithFragment { idx, fragment }, Position::Alternative(alt)) => {
        //             let before = rhs_count.get(&fragment).unwrap().range((0, 0) .. (idx, 0)).count();
        //             path.position[0] = Position::IdxWithFragment { idx: 0, fragment };
        //             path.position[1] = Position::Alternative(alt + before);
        //         }
        //         (Position::IdxWithFragment { idx, fragment }, _) => {
        //             let count_for_fragment = rhs_count.get(&fragment).unwrap();
        //             let before = count_for_fragment.range((0, 0) .. (idx, 0)).count();
        //             let total = count_for_fragment.len();
        //             path.position[0] = Position::IdxWithFragment { idx: 0, fragment };
        //             if total > 1 {
        //                 path.position.insert(1, Position::Alternative(before));
        //             }
        //         }
        //         _ => unreachable!()
        //     }
        //     path
        // }).collect()
        self.paths.clone().into_iter().map(|path| path.split_stmt()).collect()
    }
}
