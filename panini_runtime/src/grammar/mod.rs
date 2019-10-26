use std::collections::BTreeMap;

use crate::enum_stream::EnumStreamGrammar;

pub use macros::*;

#[macro_use]
mod macros;

pub struct Grammar<N, T> {
    pub rules: BTreeMap<String, Rule<N>>,
    pub sub: EnumStreamGrammar<T>,
}

pub enum Rule<T> {
    Call(String),
    Bound(Box<Rule<T>>, String),
    Repeat(Box<Rule<T>>),
    Sum(Vec<Summand<T>>),
}

pub struct Summand<T> {
    rule: Rule<T>,
    action: Box<dyn Fn(Vec<T>) -> T>,
}

impl<N, T> Grammar<N, T> {
    pub fn new(sub: EnumStreamGrammar<T>) -> Self {
        Grammar {
            rules: BTreeMap::new(),
            sub,
        }
    }

    pub fn rule(&mut self, lhs: &str, rhs: Rule<N>) {
        self.rules.insert(lhs.to_string(), rhs);
    }

    // fn sub(&mut self, grammar: EnumStreamGrammar<T>) {
    //     self.sub = Some(grammar);
    // }
}

impl<T> Rule<T> {
    pub fn call(rhs: &str) -> Rule<T> {
        Rule::Call(rhs.to_string())
    }

    pub fn repeat(self) -> Rule<T> {
        Rule::Repeat(Box::new(self))
    }

    pub fn bind(self, bound: &str) -> Rule<T> {
        Rule::Bound(Box::new(self), bound.to_string())
    }

    pub fn action(self, func: impl Fn(Vec<T>) -> T + 'static) -> Rule<T> {
        Rule::Sum(vec![
            Summand {
                rule: self,
                action: Box::new(func),
            }
        ])
    }

    fn or(self, last: Rule<T>) -> Rule<T> {
        let (mut summands, summands_b) = (self.summands(), last.summands());
        summands.extend(summands_b.into_iter());
        Rule::Sum(summands)
    }

    fn summands(self) -> Vec<Summand<T>> {
        if let Rule::Sum(summands) = self {
            summands
        } else {
            unreachable!()
        }
    }
}
