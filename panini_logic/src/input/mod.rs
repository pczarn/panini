pub mod attr_arguments;
pub mod ast;

pub use self::ast::{Stmts, Stmt, Rhs, RhsElement, RhsAst, Action, Sequence};

pub type IdentId = u32;
pub type ExprId = u32;
pub type AttrId = u32;
pub type LexerId = u32;
pub type TyId = u32;
pub type BindId = u32;
pub type FragmentId = u32;
pub type SpanId = u32;

pub const LEXER_START_FRAGMENT: FragmentId = !0 - 1;
