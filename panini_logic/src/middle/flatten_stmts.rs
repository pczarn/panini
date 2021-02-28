use std::borrow::Cow;
use std::cmp::{Ord, Ordering, PartialOrd};
use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;

use input::ast::{Rhs, RhsAst, RhsElement, Stmts};
use input::{BindId, FragmentId};

use self::input::{InputTree, Pathway, Step};

pub struct FlattenStmts {
    pub pathways: Vec<Pathway>,
}

impl Pathway {
    pub fn prefixes<'a>(&'a self) -> impl Iterator<Item = Pathway> + DoubleEndedIterator + 'a {
        (1..self.steps.len()).map(move |len| Pathway {
            steps: self.steps[..len].to_vec(),
        })
    }

    pub fn split_last(&self) -> (Step, Pathway) {
        let (last, prefix) = self.steps.split_last().unwrap();
        let prefix_pathway = Pathway {
            steps: prefix.to_vec(),
        };
        (*last, prefix_pathway)
    }

    pub fn eliminate_binds(&mut self) {
        self.steps.retain(|elem| !elem.is_bind());
    }

    pub fn new_rule(&self, prefix: &Pathway) -> Option<Pathway> {
        if self.steps[prefix.steps.len()..]
            .iter()
            .any(|pos| pos.is_sequence())
        {
            let from_pathway = prefix.clone();
            Some(from_pathway)
        } else {
            None
        }
    }

    // fn canonicalize(&self) -> Cow<Vec<Position>> {
    //     match (self.steps.get(0), self.steps.get(1)) {
    //         (Some(&Step::StmtFragment(fragment)), Some(&Step::StmtIdx(idx))) => {
    //             let mut position = self.steps.clone();
    //             position[0] = Step::IdxWithFragment { idx, fragment };
    //             position.remove(1);
    //             Cow::Owned(position)
    //         }
    //         // (Some(&Step::StmtFragment(fragment)), None) => {
    //         //     Cow::Owned(vec![Step::IdxWithFragment { fragment, idx: 0 }])
    //         // }
    //         _ => Cow::Borrowed(&self.steps)
    //     }
    // }

    // fn split_stmt(mut self) -> Pathway {
    //     match self.steps[0] {
    //         Step::IdxWithFragment { idx, fragment } => {
    //             self.steps[0] = Step::StmtFragment(fragment);
    //             self.steps.insert(1, Step::StmtIdx(idx));
    //         }
    //         _ => {}
    //     }
    //     self
    // }

    pub fn range(self) -> Range<Pathway> {
        let mut end = self.clone();
        end.steps.push(Step::Max);
        self..end
    }

    fn prefix(&self, n: usize) -> Pathway {
        Pathway {
            steps: self.steps[..n].to_vec(),
        }
    }
}

impl Step {
    fn is_bind(&self) -> bool {
        match self {
            &Step::Bind { .. } => true,
            _ => false,
        }
    }

    fn is_sequence(&self) -> bool {
        match self {
            &Step::Sequence { .. } => true,
            _ => false,
        }
    }
}

impl FlattenStmts {
    pub fn new() -> Self {
        FlattenStmts { pathways: vec![] }
    }

    pub fn flatten_stmts(&mut self, stmts: &Stmts) {
        for (stmt_idx, stmt) in stmts.stmts.iter().enumerate() {
            for (alternative_idx, (_level, ref rhs, ref _action)) in stmt.body.iter().enumerate() {
                let mut pathway = pathway![
                    Step::StmtFragment(stmt.lhs),
                    Step::StmtIdx(stmt_idx),
                    Step::Alternative(alternative_idx),
                ];
                self.flatten_rhs(pathway, rhs);
            }
        }
        self.simplify();
    }

    fn flatten_rhs(&mut self, pathway: Pathway, rhs: &Rhs) {
        if rhs.0.is_empty() {
            self.pathways.push(pathway);
            return;
        }
        for (rhs_idx, element) in rhs.0.iter().enumerate() {
            let mut pathway = pathway.clone();
            if let Some(bind_id) = element.bind {
                pathway.steps.push(Step::Bind {
                    bind_id,
                    idx: rhs_idx,
                });
            } else {
                pathway.steps.push(Step::Idx(rhs_idx));
            }
            match &element.elem {
                &RhsAst::Fragment(fragment_id) => {
                    pathway.steps.push(Step::Fragment(fragment_id));
                    self.pathways.push(pathway);
                }
                &RhsAst::String(ref _string) => {
                    // pathway.steps.push(Step::IdxWithString {
                    //     idx: rhs_idx,
                    //     string,
                    // });
                    // self.pathways.push(pathway);
                    unimplemented!()
                }
                &RhsAst::Sequence(ref sequence) => {
                    pathway.steps.push(Step::Sequence {
                        min: sequence.min,
                        max: sequence.max,
                    });
                    self.flatten_rhs(pathway, &sequence.rhs);
                }
                &RhsAst::Sum(ref summands) => {
                    if summands.is_empty() {
                        self.pathways.push(pathway.clone());
                    }
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let mut pathway = pathway.clone();
                        pathway.steps.push(Step::Alternative(summand_idx));
                        self.flatten_rhs(pathway, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
                    self.flatten_rhs(pathway, rhs);
                }
            }
        }
    }

    fn simplify(&mut self) {
        let mut path_set: BTreeSet<Pathway> = self.pathways.iter().cloned().collect();
        let mut remove = vec![];
        for i in 0..self.pathways.len() {
            for j in 0..self.pathways[i].steps.len() {
                match self.pathways[i].steps[j] {
                    Step::Alternative(idx) if idx == 0 => {
                        let mut start = self.pathways[i].prefix(j);
                        let mut end = start.clone();
                        start.steps.push(Step::Alternative(1));
                        end.steps.push(Step::Alternative(!0));
                        if path_set.range(start..end).count() == 0 {
                            remove.push((i, j));
                        }
                    }
                    Step::Idx(idx) if idx == 0 => {
                        let mut start = self.pathways[i].prefix(j);
                        let mut end = start.clone();
                        start.steps.push(Step::Idx(1));
                        end.steps.push(Step::Idx(!0));
                        if path_set.range(start..end).count() == 0 {
                            remove.push((i, j));
                        }
                    }
                    Step::StmtIdx(idx) if idx == 0 => {
                        let mut start = self.pathways[i].prefix(j);
                        let mut end = start.clone();
                        start.steps.push(Step::StmtIdx(1));
                        end.steps.push(Step::StmtIdx(!0));
                        if path_set.range(start..end).count() == 0 {
                            remove.push((i, j));
                        }
                    }
                }
            }
        }
        for (i, j) in remove {
            self.pathways[i].steps.remove(j);
        }
    }

    pub fn pathways(self) -> Vec<Pathway> {
        self.pathways
    }
}
