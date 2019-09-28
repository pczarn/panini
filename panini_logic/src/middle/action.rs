use std::slice;

use input::{IdentId, ExprId};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Action {
    Tuple {
        tuple_binds: Vec<usize>,
    },
    Struct {
        deep_binds: Vec<usize>,
        shallow_binds: Vec<(usize, IdentId)>,
        expr: ActionExpr,
    },
    Sequence,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ActionExpr {
    // Builds a tuple or struct
    Auto,
    // Action
    Inline {
        expr: ExprId,
    },
}

impl ActionExpr {
    pub fn is_inline(&self) -> bool {
        *self != ActionExpr::Auto
    }
}

// Someday, generators will be available. Use them here.
impl Action {
    pub fn directly_bound_positions(&self) -> DirectlyBoundPositions {
        match self {
            &Action::Tuple { ref tuple_binds } => {
                DirectlyBoundPositions::List(tuple_binds.iter())
            }
            &Action::Struct { ref deep_binds, ref shallow_binds, .. } => {
                DirectlyBoundPositions::Chain(deep_binds.iter(), shallow_binds.iter())
            }
            _ => {
                // No other action has _directly_ bound positions.
                DirectlyBoundPositions::List([].iter())
            }
        }
    }
}

pub enum DirectlyBoundPositions<'a> {
    List(slice::Iter<'a, usize>),
    Chain(slice::Iter<'a, usize>, slice::Iter<'a, (usize, IdentId)>),
}

impl<'a> Iterator for DirectlyBoundPositions<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            &mut DirectlyBoundPositions::List(ref mut iter) => {
                iter.next().cloned()
            }
            &mut DirectlyBoundPositions::Chain(ref mut a, ref mut b) => {
                a.next().cloned().or_else(|| b.next().map(|&(pos, _)| pos))
            }
        }
    }
}
