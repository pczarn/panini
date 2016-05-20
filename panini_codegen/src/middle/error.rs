use std::error::Error;
use std::fmt;

use rs;

pub use self::TransformationError::*;

pub type Name = rs::Spanned<rs::Name>;

#[derive(Debug)]
pub enum TransformationError {
    GrammarIsEmpty,
    RecursiveType(Vec<(Name, Vec<Name>)>),
    TypeMismatch,
    InvalidAttr(rs::Span),
}

impl fmt::Display for TransformationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for TransformationError {
    fn description(&self) -> &str {
        match *self {
            GrammarIsEmpty => "the grammar is empty.",
            RecursiveType(_) => "recursive type.",
            TypeMismatch => "type mismatch.",
            InvalidAttr(_) => "invalid attribute.",
        }
    }
}
