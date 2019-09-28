pub mod lexer;
pub mod visit;

use rs;

// pub use self::ast::{Stmts, Stmt, Rhs, RhsElement, RhsAst, Action, Sequence};

pub type Name = rs::Spanned<rs::Term>;
