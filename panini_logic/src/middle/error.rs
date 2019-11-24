use std::error::Error;
use std::fmt;

use cfg::Symbol;

pub use self::TransformationError::*;

// pub type Name = rs::Spanned<rs::Name>;

#[derive(Debug, Eq, PartialEq)]
pub enum TransformationError {
    GrammarIsEmpty,
    RecursiveType(Vec<CycleWithCauses>),
    TypeMismatch(Vec<(usize, usize)>),
    InvalidAttr(),
}

impl fmt::Display for TransformationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Error for TransformationError {
    fn description(&self) -> &str {
        match *self {
            GrammarIsEmpty => "the grammar is empty.",
            RecursiveType(_) => "recursive type.",
            TypeMismatch(..) => "type mismatch.",
            InvalidAttr(..) => "invalid attribute.",
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct CycleWithCauses {
    pub lhs: Symbol,         // Spanned
    pub causes: Vec<Symbol>, // Spanned
}
