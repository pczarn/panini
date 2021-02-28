use std::collections::BTreeMap;
use std::rc::Rc;

use panini_logic::input::ast::{
    Action, Alternative, Rhs, RhsAst, RhsElement, Sequence, Stmt, Stmts,
};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::input::{ExprId, FragmentId};
use panini_logic::output::instruction::InstructionList;

use crate::enum_stream::EnumStreamGrammar;
use crate::grammar::{Grammar, Rule};

pub struct Tables<N> {
    fragments: BTreeMap<String, FragmentId>,
    fragment_vec: Vec<String>,
    action_vec: Vec<Rc<dyn Fn(Vec<N>) -> N>>,
}

pub struct Parser<N, T> {
    pub(super) tables: Tables<N>,
    instruction_list: InstructionList,
    sub: EnumStreamGrammar<T>,
}

pub struct ParseResult<N, T>(N, T); // TODO

impl<N> Tables<N> {
    pub(super) fn new() -> Self {
        Tables {
            fragments: BTreeMap::new(),
            fragment_vec: vec![],
            action_vec: vec![],
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
    pub(super) fn new(sub: EnumStreamGrammar<T>) -> Parser<N, T> {
        Parser {
            tables: Tables::new(),
            instruction_list: InstructionList::new(),
            sub: sub,
        }
    }

    pub(super) fn process(&mut self, input_tree: InputTree) {
        self.instruction_list = panini_logic::process(input_tree).unwrap();
    }

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
