use input::{AttrId, TyId, LexerId, BindId, FragmentId, ExprId, SpanId};
use input::attr_arguments::AttrArguments;

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
    Fragment(rs::Term),
    StmtFragment(rs::Term),
    StmtTy(rs::TokenStream),
    StmtIdx(usize),
    // Class(Class, Symbol),
    // RightQuote,
    Bind { bind_id: BindId, idx: usize },
    Sequence {
        min: u32,
        max: Option<u32>,
    },
    SequenceEnd,
    SequenceToken,
    Max,
}
