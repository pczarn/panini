use std::collections::BTreeMap;
use std::rc::Rc;

use crate::enum_stream::EnumStreamGrammar;

pub use macros::*;

#[macro_use]
mod macros;

pub struct Grammar<N, T> {
    // pub rules: BTreeMap<String, Rule<N>>,
    pub sub: EnumStreamGrammar<T>,
    pub tables: Tables<N>,
    pub pathway_graph: PathwayGraph,
}

pub struct Tables<N> {
    fragments: BTreeMap<String, FragmentId>,
    fragment_vec: Vec<String>,
    action_vec: Vec<Rc<dyn Fn(Vec<N>) -> N>>,
    stmt_map: BTreeMap<(FragmentId, usize), ExprId>,
}

pub struct Parser<N, T> {
    instruction_list: InstructionList,
    grammar: Grammar<N, T>,
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
            sub,
            tables: Tables::new(),
            pathway_graph: PathwayGraph::new(),
        }
    }

    pub fn rule(&mut self, lhs: &str, rhs: Rule<N>) {
        self.lower_rhs(Some(lhs.to_string()), rhs);
    }

    fn lower_rhs(&mut self, lhs: Option<String>, rule: Rule<N>) -> Vec<StepId> {
        assert!(lhs.none() ^ rule.is_sum());
        match rule {
            Rule::Sum(summands) => {
                vec![self.pathway_graph.node(
                    Step::StmtFragment(lhs),
                    self.lower_summands(lhs.unwrap(), summands),
                )]
            }
            Rule::Bound(inner_rule, bound) => {
                let (bind_id, idx) = (0, 0);
                vec![self.pathway_graph.node(
                    Step::Bind { bind_id, idx },
                    self.lower_rhs(None, *inner_rule),
                )]
            }
            Rule::Call(callee) => {
                vec![self.pathway_graph.node(
                    Step::Fragment(self.tables.intern_fragment(callee)),
                    NULL_NODE,
                )]
            }
            Rule::Repeat(inner_rule) => {
                let (min, max) = (0, None);
                vec![self.pathway_graph.node(
                    Step::Sequence { min, max },
                    self.lower_rhs(None, *inner_rule),
                )]
            }
            Rule::Product(factors) => {
                factors.into_iter().flat_map(|factor| self.lower_rhs(None, factor).into_iter()).collect()
            }
        }
    }

    fn lower_summands(&mut self, summands: Vec<Summand<N>>) -> Vec<StepId> {
        for (i, summand) in summands.iter().enumerate() {
            self.tables.insert_stmt(lhs, i, self.tables.intern_action(summand.action));
        }
        summands.into_iter().enumerate().map(|(i, summand)|
            self.pathway_graph.node(
                Step::StmtIdx(i),
                self.lower_rhs(summand.rule),
            )
        ).collect()
    }

    pub fn parser(self) -> Parser<N, T> {
        let input_tree = InputTree {
            attr_arguments: AttrArguments {
                lexer_arguments: None,
            },
            pathway_graph: self.pathway_graph.clone(),
            lexer: Some(0),
        };
        let instruction_list = panini_logic::process(input_tree).unwrap();
        let mut parser = Parser {
            instruction_list,
            grammar: self,
        };
        parser
    }
}

pub struct ParseResult<N, T>(N, T); // TODO

impl<N> Tables<N> {
    pub(super) fn new() -> Self {
        Tables {
            fragments: BTreeMap::new(),
            fragment_vec: vec![],
            action_vec: vec![],
            stmt_map: BTreeMap::new(),
        }
    }

    pub(super) fn intern_fragment(&mut self, elem: String) -> FragmentId {
        let fragment_vec = &mut self.fragment_vec;
        *self.fragments.entry(elem.clone()).or_insert_with(|| {
            let id = fragment_vec.len() as FragmentId;
            fragment_vec.push(elem);
            id
        })
    }

    pub(super) fn intern_action(&mut self, action: Rc<dyn Fn(Vec<N>) -> N>) -> ExprId {
        let expr = self.action_vec.len() as ExprId;
        self.action_vec.push(action);
        expr
    }
}
impl<N, T> Parser<N, T>
    where
        T: Copy,
{
    pub fn parse(&self, tokens: &[T]) -> ParseResult<N, T> {
        for &token in tokens {
            for terminal in self.sub.eval(token) {
                println!("{}", self.tables.fragments.get(&terminal).unwrap());
            }
        }
        // let recognizer = ;
        unimplemented!()
        // for &token in tokens {
        //     recognizer.begin_earleme();
        //     for terminal in self.sub.eval(token) {
        //         let fragment = self.fragments[terminal];
        //         recognizer.scan();
        //     }
        //     recognizer.end_earleme();
        // }
    }
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

    fn is_sum(&self) -> bool {
        if let &Rule::Sum(_) = self {
            true
        } else {
            false
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
