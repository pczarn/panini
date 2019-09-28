#![allow(dead_code)]

use cfg::*;

use gearley::grammar::History;

pub fn assert_eq_rules<R1, R2, I, J>(i: I, j: J)
    where R1: GrammarRule,
          R2: GrammarRule,
          I: Iterator<Item=R1>,
          J: Iterator<Item=R2>
{
    let rules_i = i.map(|rule| (rule.lhs(), rule.rhs().to_vec())).collect::<Vec<_>>();
    let rules_j = j.map(|rule| (rule.lhs(), rule.rhs().to_vec())).collect::<Vec<_>>();

    assert_eq!(rules_i, rules_j);
}

pub fn assert_eq_origins<R, I>(i: I, j: Vec<Option<u32>>)
    where R: GrammarRule<History=History>,
          I: Iterator<Item=R>
{
    let origins_i = i.map(|rule| rule.history().origin()).collect::<Vec<_>>();
    assert_eq!(origins_i, j);
}
