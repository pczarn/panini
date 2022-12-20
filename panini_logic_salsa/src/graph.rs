use std::collections::BTreeSet;

#[derive(Clone, Debug)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<Edge>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Idx(IdxKind, usize),
    Fragment(FragmentId),
    StmtFragment(FragmentId),
    StmtTy(TyId),
    // Class(Class, Symbol),
    // RightQuote,
    Bind { bind_id: BindId, idx: usize },
    Sequence { min: u32, max: Option<u32> },
    SequenceEnd,
    SequenceToken,
    Max,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Edge {
    Forward {
        a: usize,
        b: usize,
    },
    Backward {
        b: usize,
        a: usize,
    },
}



#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum IdxKind {
    Sum,
    Product,
    Stmt,
}

pub type IdentId = u32;
pub type ExprId = u32;
pub type AttrId = u32;
pub type LexerId = u32;
pub type TyId = u32;
pub type BindId = u32;
pub type FragmentId = u32;
pub type SpanId = u32;
pub type StepId = usize;

pub const NULL_NODE: usize = 0;
