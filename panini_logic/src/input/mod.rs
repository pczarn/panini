pub mod ast;
pub mod parameters;

pub use self::ast::{PathwayGraph, Step};
pub use self::parameters::{Parameters, LexerParameters};

pub type IdentId = u32;
pub type ExprId = u32;
pub type AttrId = u32;
pub type LexerId = u32;
pub type TyId = u32;
pub type BindId = u32;
pub type FragmentId = u32;
pub type SpanId = u32;

pub const LEXER_START_FRAGMENT: FragmentId = !0 - 1;

pub struct Input {
    pub pathway_graph: PathwayGraph,
    pub parameters: Parameters,
    pub lexer: Option<LexerId>,
}
