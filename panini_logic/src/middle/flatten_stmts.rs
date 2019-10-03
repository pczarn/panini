use input::{BindId, FragmentId};
use input::ast::{Stmts, RhsAst, Rhs};

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Path {
    pub position: Vec<Position>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Position {
    Idx(usize),
    IdxWithFragment {
        idx: usize,
        fragment: FragmentId,
    },
    Bind(BindId),
    Alternative(usize),
    Sequence {
        min: u32,
        max: Option<u32>,
    },
    SequenceToken,
    Max,
}

pub struct FlattenStmts {
    pub paths: Vec<Path>,
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
                let path = Path {
                    position: vec![
                        Position::IdxWithFragment {
                            idx: stmt_idx,
                            fragment: stmt.lhs,
                        },
                        Position::Alternative(alternative_idx),
                    ],
                };
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
                &RhsAst::Fragment(fragment_id) => {
                    path.position.push(Position::IdxWithFragment {
                        idx: rhs_idx,
                        fragment: fragment_id,
                    });
                    self.paths.push(path);
                }
                &RhsAst::Sequence(ref sequence) => {
                    path.position.push(Position::Idx(rhs_idx));
                    path.position.push(Position::Sequence {
                        min: sequence.min,
                        max: sequence.max,
                    });
                    self.flatten_rhs(path, &sequence.rhs);
                }
                &RhsAst::Sum(ref summands) => {
                    path.position.push(Position::Idx(rhs_idx));
                    if summands.is_empty() {
                        self.paths.push(path.clone());
                    }
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let mut path = path.clone();
                        path.position.push(Position::Alternative(summand_idx));
                        self.flatten_rhs(path, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
                    path.position.push(Position::Idx(rhs_idx));
                    self.flatten_rhs(path, rhs);
                }
            }
        }
        if rhs.0.is_empty() {
            self.paths.push(path);
        }
    }
}

impl Path {
    pub fn split_last(&self) -> (&Position, Path) {
        let (last, prefix) = self.position.split_last().unwrap();
        let prefix_path = Path { position: prefix.to_vec() };
        (last, prefix_path)
    }
}
