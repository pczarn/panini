pub mod action;
pub mod attr;
pub mod error;
pub mod embedded_string;
pub mod fold;
pub mod hir;
pub mod ir;
pub mod rule;
pub mod trace;
pub mod ty;
pub mod lint;
pub mod warn;

pub use self::ir::{Ir, InvocationOfInnerLayer};
pub use self::hir::{Hir, SymbolicName, SpannedSymbolicName};
pub use self::action::{Action, ActionExpr};
pub use self::fold::{Folder, FoldHir};
pub use self::ty::{Ty, AutoTy};
pub use self::rule::{Rule, FoldRule};
pub use lexer::Lexer;

use rs;

// pub type Name = rs::Spanned<rs::Name>;
