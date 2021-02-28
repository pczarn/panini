use std::collections::BTreeMap;
use std::rc::Rc;

use crate::enum_stream::EnumStreamGrammar;

pub use macros::*;

#[macro_use]
mod macros;
mod lower;
mod parser;

pub struct Grammar<N, T> {
    pub rules: BTreeMap<String, Rule<N>>,
    pub sub: EnumStreamGrammar<T>,
}

pub enum Rule<T> {
    Call(String),
    Bound(Box<Rule<T>>, String),
    Repeat(Box<Rule<T>>),
    Product(Vec<Rule<T>>),
    Sum(Vec<Summand<T>>),
}

pub struct Summand<T> {
    rule: Rule<T>,
    action: Rc<dyn Fn(Vec<T>) -> T>,
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
        Rule::Sum(vec![Summand {
            rule: self,
            action: Rc::new(func),
        }])
    }

    fn or(self, next: Rule<T>) -> Rule<T> {
        let (mut summands, summands_b) = (self.summands(), next.summands());
        summands.extend(summands_b.into_iter());
        Rule::Sum(summands)
    }

    pub fn then(self, next: Rule<T>) -> Rule<T> {
        let (mut factors, factors_b) = (self.factors(), next.factors());
        factors.extend(factors_b.into_iter());
        Rule::Product(factors)
    }

    fn summands(self) -> Vec<Summand<T>> {
        if let Rule::Sum(summands) = self {
            summands
        } else {
            unreachable!()
        }
    }

    fn factors(self) -> Vec<Rule<T>> {
        if let Rule::Product(factors) = self {
            factors
        } else {
            vec![self]
        }
    }
}
