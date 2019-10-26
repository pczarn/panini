use std::collections::BTreeMap;

pub use macros::*;

#[macro_use]
mod macros;

#[derive(Eq, PartialEq, Debug)]
pub struct EnumStreamGrammar<T> {
    patterns: BTreeMap<String, Pattern<T>>,
}

pub enum Pattern<T> {
    Pat(Box<dyn Fn(T) -> bool>),
    Negate(Box<Pattern<T>>),
    Conjunction(Vec<Pattern<T>>),
}

impl<T> ::std::fmt::Debug for Pattern<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            &Pattern::Pat(..) => {
                write!(f, "Pattern::Pat(..)")
            }
            &Pattern::Negate(ref neg) => {
                write!(f, "Pattern::Negate({:?})", neg)
            }
            &Pattern::Conjunction(ref summands) => {
                write!(f, "Pattern::Conjunction({:?})", summands)
            }
        }
    }
}

impl<T> ::std::cmp::PartialEq for Pattern<T> {
    fn eq(&self, other: &Pattern<T>) -> bool {
        match (self, other) {
            (&Pattern::Pat(..), &Pattern::Pat(..)) => true,
            (&Pattern::Negate(ref a), &Pattern::Negate(ref b)) => a == b,
            (&Pattern::Conjunction(ref a), &Pattern::Conjunction(ref b)) => a == b,
            _ => false,
        }
    }
}

impl<T> ::std::cmp::Eq for Pattern<T> {}

impl<T> EnumStreamGrammar<T> {
    pub fn new() -> Self {
        EnumStreamGrammar {
            patterns: BTreeMap::new(),
        }
    }
    
    pub fn pattern(&mut self, lhs: &str, pat: Pattern<T>) {
        self.patterns.insert(lhs.to_string(), pat);
    }
}

impl<T> Pattern<T> {
    pub fn pattern(func: impl Fn(T) -> bool + 'static) -> Pattern<T> {
        Pattern::Pat(Box::new(func))
    }

    fn negate(self) -> Pattern<T> {
        Pattern::Negate(Box::new(self))
    }

    fn and(self, last: Pattern<T>) -> Pattern<T> {
        let (mut summands, summands_b) = (self.summands(), last.summands());
        summands.extend(summands_b.into_iter());
        Pattern::Conjunction(summands)
    }

    fn summands(self) -> Vec<Pattern<T>> {
        if let Pattern::Conjunction(summands) = self {
            summands
        } else {
            vec![self]
        }
    }
}
