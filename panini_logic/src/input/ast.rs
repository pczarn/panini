use input::attr_arguments::AttrArguments;
use input::{AttrId, BindId, ExprId, FragmentId, LexerId, SpanId, TyId};

pub struct InputTree {
    pub pathways: Vec<Pathway>,

    pub attr_arguments: AttrArguments,
    pub lexer: Option<LexerId>,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Pathway {
    pub steps: Vec<Step>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Alternative(usize),
    Idx(usize),
    Fragment(FragmentId),
    StmtFragment(FragmentId),
    StmtTy(TyId),
    StmtIdx(usize),
    // Class(Class, Symbol),
    // RightQuote,
    Bind { bind_id: BindId, idx: usize },
    Sequence { min: u32, max: Option<u32> },
    SequenceEnd,
    SequenceToken,
    Max,
}
