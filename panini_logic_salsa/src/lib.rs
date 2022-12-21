use std::collections::btree_set::Range;
use std::collections::{BTreeMap, BTreeSet};
use std::mem;
use std::rc::Rc;

use elsa::FrozenIndexSet;
use indexmap::IndexSet;
use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;

use graph::{BindId, FragmentId, IdxKind, NodeId, PathwayGraph, Step};

extern crate salsa;

mod graph;

#[derive(Clone)]
struct ComparableTokenStream(TokenStream);

impl std::fmt::Debug for ComparableTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for ComparableTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for ComparableTokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.to_string().eq(&other.to_string())
    }
}

impl Eq for ComparableTokenStream {
    fn assert_receiver_is_total_eq(&self) {}
}

impl std::hash::Hash for ComparableTokenStream {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state)
    }
}

impl From<TokenStream> for ComparableTokenStream {
    fn from(token_stream: TokenStream) -> Self {
        ComparableTokenStream(token_stream)
    }
}

impl From<ComparableTokenStream> for TokenStream {
    fn from(token_stream: ComparableTokenStream) -> Self {
        token_stream.0
    }
}

#[salsa::query_group(ProvideInputStorage)]
trait ProvideInput {
    #[salsa::input]
    fn input(&self) -> Rc<Input> {
        self.input.clone()
    }

    fn pretty_input(&self) -> String;

    fn tokenize_input(&self) -> ComparableTokenStream;

    fn tokenize_node(&self, node_id: NodeId) -> ComparableTokenStream;

    fn tokenize_each_child(&self, node_id: NodeId) -> Vec<ComparableTokenStream>;

    fn need_parentheses_around_children(&self, node_id: NodeId) -> bool;

    fn get_node(&self, node_id: NodeId) -> Option<Step>;

    fn get_sym(&self, fragment_id: FragmentId) -> String;

    fn tokenize_children(
        &self,
        node_id: NodeId,
        separator: Option<ComparableTokenStream>,
        may_add_parentheses: bool,
    ) -> ComparableTokenStream;

    fn get_bind(&self, bind_id: BindId) -> String;
}

fn get_node(db: &dyn ProvideInput, node_id: NodeId) -> Option<Step> {
    db.input().graph.get(node_id)
}

fn get_bind(db: &dyn ProvideInput, bind_id: BindId) -> String {
    let input = db.input();
    let mut nth = input.bind_set.iter().nth(bind_id as usize);
    nth.as_ref()
        .expect("incorrect id in Bind { bind_id }")
        .to_string()
}

fn get_sym(db: &dyn ProvideInput, fragment_id: FragmentId) -> String {
    let input = db.input();
    let mut nth = input.lhs_set.iter().nth(fragment_id as usize);
    nth.as_ref()
        .expect("incorrect id in Fragment(fragment_id)")
        .to_string()
}

fn tokenize_each_child(db: &dyn ProvideInput, node_id: NodeId) -> Vec<ComparableTokenStream> {
    db.input()
        .graph
        .children(node_id)
        .map(|child_id| db.tokenize_node(child_id))
        .collect()
}

fn need_parentheses_around_children(db: &dyn ProvideInput, node_id: NodeId) -> bool {
    db.input()
        .graph
        .children(node_id)
        .enumerate()
        .any(|(i, id)| match (i, db.input().graph.get(id).unwrap()) {
            (0, Step::Fragment(..)) => false,
            (0, Step::Sequence { .. }) => false,
            _ => true,
        })
}

fn tokenize_children(
    db: &dyn ProvideInput,
    node_id: NodeId,
    separator: Option<ComparableTokenStream>,
    may_add_parentheses: bool,
) -> ComparableTokenStream {
    let mut inner = TokenStream::new();
    if let Some(separator) = separator {
        inner.extend(
            db.tokenize_each_child(node_id)
                .into_iter()
                .intersperse(separator)
                .map(|s| s.0),
        );
    } else {
        inner.extend(db.tokenize_each_child(node_id).into_iter().map(|s| s.0));
    }
    let add_parentheses = db.need_parentheses_around_children(node_id) && may_add_parentheses;
    if add_parentheses {
        quote! { ( #inner ) }.into()
    } else {
        inner.into()
    }
}

fn tokenize_node(db: &dyn ProvideInput, node_id: NodeId) -> ComparableTokenStream {
    match db.get_node(node_id).expect("invalid node_id") {
        Step::Fragment(fragment_id) => {
            let ident = db.get_sym(fragment_id);
            let mut result = TokenStream::new();
            result.extend(vec![TokenTree::Ident(Ident::new(
                &ident[..],
                Span::call_site(),
            ))]);
            result
        }
        Step::Sequence { min: 0, max: None } => {
            let children: TokenStream = db.tokenize_children(node_id, None, true).into();
            quote! { #children * }
        }
        Step::Idx(IdxKind::Sum, _idx) => db
            .tokenize_children(node_id, Some(quote! { | }.into()), false)
            .into(),
        Step::Idx(IdxKind::Product, _idx) => db.tokenize_children(node_id, None, false).into(),
        Step::Bind { bind_id, idx: _ } => {
            let bind = db.get_bind(bind_id);
            let bind_ident = TokenTree::Ident(Ident::new(&bind[..], Span::call_site()));
            let children: TokenStream = db.tokenize_children(node_id, None, true).into();
            quote! { #bind_ident : #children }
        }
        Step::StmtFragment(fragment_id) => {
            let lhs_ident = db.get_sym(fragment_id);
            let lhs = TokenTree::Ident(Ident::new(&lhs_ident[..], Span::call_site()));
            let rhs: TokenStream = db.tokenize_children(node_id, None, false).into();
            quote! { #lhs ::= #rhs ; }
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
    .into()
}

fn tokenize_input(db: &dyn ProvideInput) -> ComparableTokenStream {
    let mut result = TokenStream::new();
    for root_id in db.input().graph.roots() {
        result.extend(db.tokenize_node(root_id).0);
    }
    result.into()
}

fn pretty_input(db: &dyn ProvideInput) -> String {
    db.tokenize_input().to_string()
}

#[salsa::database(ProvideInputStorage)]
#[derive(Default)]
struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}

#[derive(Clone)]
pub struct Input {
    graph: PathwayGraph,
    lhs_set: IndexSet<String>,
    bind_set: IndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

pub struct FrozenInput {
    graph: PathwayGraph,
    lhs_set: FrozenIndexSet<String>,
    bind_set: FrozenIndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

#[derive(Debug)]
pub struct InputDebug<'a> {
    graph: &'a PathwayGraph,
    lhs_set: Vec<String>,
    bind_set: Vec<String>,
    rule_indices: &'a BTreeMap<String, usize>,
}

impl ::std::fmt::Debug for Input {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        fn interner_to_vec(set: &IndexSet<String>) -> Vec<String> {
            let mut result = vec![];
            for i in 0usize.. {
                if let Some(elem) = set.get_index(i) {
                    result.push(elem.to_string());
                } else {
                    break;
                }
            }
            result
        }
        InputDebug {
            graph: &self.graph,
            lhs_set: interner_to_vec(&self.lhs_set),
            bind_set: interner_to_vec(&self.bind_set),
            rule_indices: &self.rule_indices,
        }
        .fmt(fmt)
    }
}

impl FrozenInput {
    fn new() -> Self {
        FrozenInput {
            graph: PathwayGraph::new(),
            lhs_set: FrozenIndexSet::new(),
            bind_set: FrozenIndexSet::new(),
            rule_indices: BTreeMap::new(),
        }
    }

    fn intern_fragment(&mut self, ident: &str) -> FragmentId {
        self.lhs_set.insert_full(ident.to_string()).0 as FragmentId
    }

    fn intern_bind(&mut self, ident: &str) -> FragmentId {
        self.bind_set.insert_full(ident.to_string()).0 as FragmentId
    }

    fn next_stmt_idx(&mut self, lhs_str: &str) -> usize {
        let entry = self.rule_indices.entry(lhs_str.to_string()).or_insert(0);
        let result = *entry;
        *entry += 1;
        result
    }

    fn thaw(self) -> Input {
        Input {
            graph: self.graph,
            lhs_set: self.lhs_set.into_set(),
            bind_set: self.bind_set.into_set(),
            rule_indices: self.rule_indices,
        }
    }
}

macro_rules! input {
    (
        $(
            $lhs:ident
            ::=
            $(
                $rule:tt
            )|+
            ;
        )*
    ) => (
        {
            let mut input = FrozenInput::new();
            $(
                let lhs_str = stringify!($lhs);
                let args = vec![
                    $(
                        (
                            Step::Idx(IdxKind::Sum, input.next_stmt_idx(lhs_str)),
                            rule!(input: input, $rule),
                        ),
                    )*
                ];
                let children = args.into_iter().map(
                    |(step, children_vec)| {
                        input.graph.node(step, children_vec)
                    }
                ).collect();
                let step = Step::StmtFragment(input.intern_fragment(lhs_str));
                input.graph.node(
                    step,
                    children,
                );
            )*
            input.thaw()
        }
    )
}

macro_rules! rule {
    (input: $input:expr, $rhs:ident) => {
        {
            let step = Step::Fragment($input.intern_fragment(stringify!($rhs)));
            vec![
                $input.graph.node(step, vec![])
            ]
        }
    };
    (input: $input:expr, ( $name:ident : $rhs:tt )) => {
        {
            let step = Step::Bind { bind_id: $input.intern_bind(stringify!($name)), idx: 0 };
            let children = rule!(input: $input, $rhs);
            vec![
                $input.graph.node(step, children)
            ]
        }
    };
    (input: $input:expr, ( $rhs:tt * )) => {
        {
            let step = Step::Sequence { min: 0, max: None };
            let children = rule!(input: $input, $rhs);
            vec![
                $input.graph.node(step, children)
            ]
        }
    };
    (input: $input:expr, ( $($rhs:tt)* )) => {
        {
            let children = vec![
                $(
                    rule!(input: $input, $rhs),
                )*
            ].into_iter().flat_map(|v| v.into_iter()).collect::<Vec<_>>();
            vec![
                $input.graph.node(Step::Idx(IdxKind::Product, 0), children)
            ]
            // TODO: only single node returned, not vec![ node ]
            // TODO: IdxKind::Product does not need a usize?
        }
    };
}

// macro_rules! declare_binds {
//     ([$input:ident] $rhs:ident) => ();

//     ([$input:ident] ($name:ident : $rhs:ident)) => (
//         let $name = $input.pop().unwrap().$rhs();
//     );

//     ([$input:ident] ($($rhs:tt)*)) => {
//         $(declare_binds!([$input] $rhs);)*
//     };
// }

use proc_macro2::{Delimiter, TokenTree};

#[derive(Clone, Copy, Debug)]
enum VerifyState {
    ExpectName,
    ExpectEq,
    ExpectContent,
}

#[derive(Clone, Debug)]
struct VerifyGroup {
    name: String,
    content: TokenStream,
}

impl VerifyGroup {
    fn new() -> Self {
        VerifyGroup {
            name: String::new(),
            content: TokenStream::new(),
        }
    }
}

fn verify(input: Input, tokens: TokenStream) {
    let mut groups = vec![];
    let mut what_to_expect = vec![
        VerifyState::ExpectName,
        VerifyState::ExpectEq,
        VerifyState::ExpectEq,
        VerifyState::ExpectContent,
    ]
    .into_iter()
    .cycle();
    let mut state = VerifyGroup::new();
    for (token_tree, step) in tokens.into_iter().zip(what_to_expect) {
        match (token_tree, step) {
            (TokenTree::Ident(ident), VerifyState::ExpectName) => {
                state.name = ident.to_string();
            }
            (TokenTree::Punct(punct), VerifyState::ExpectEq) => {
                assert_eq!(punct.as_char(), '=');
            }
            (TokenTree::Group(group), VerifyState::ExpectContent) => {
                assert_eq!(group.delimiter(), Delimiter::Brace);
                groups.push(mem::replace(&mut state, VerifyGroup::new()));
            }
            (token_tree, step) => {
                panic!(
                    "unexpected token tree {:?}, current state {:?}",
                    token_tree, step
                );
            }
        }
    }

    // println!("{:?}", input);

    let mut db = DatabaseStruct::default();
    db.set_input(Rc::new(input));

    for group in groups {
        match &group.name[..] {
            "input" => {
                assert_eq!(db.pretty_input(), group.content.to_string());
            }
            "flattened" => {
                unimplemented!();
            }
            "trace" => {
                unimplemented!();
            }
            "rewritten" => {
                unimplemented!();
            }
            "types" => {
                unimplemented!();
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        verify(
            input!(
                start ::= (a b);
            ),
            quote! {
                input == {
                    start ::= a b;
                }
                flattened == {
                    start@0 a@0;
                    start@0 b@1;
                }
                trace == {
                    start@0 a.0 => a;
                    start@0 b.1 => b;
                }
                rewritten == {
                    start ::= 0 a 1 b 2;
                }
                types == {
                    start: (typeof a, typeof b);
                }
                program == {

                }
            },
        );
    }

    #[test]
    fn multilevel_sequence() {
        verify(
            input!(
                start ::= ((((a:a) (b:b))*)*);
            ),
            quote! {
                input == {
                    start ::= (a:a b:b)**;
                }
                flattened == {
                    start .0 a@0;
                    start .0 b@1;
                }
                trace == {
                    // 0
                    start .0 * * => LParen;
                    // 1
                    start .0 * * .0 => a;
                    // 2
                    start .0 * * .1 => b;
                    // 3
                    start .0 * * "SequenceEnd" => RParen;
                    // 4
                    start .0 * * "SequenceToken" => *;
                    // 5
                    start .0 * "SequenceToken" => *;
                    // 6
                }
                rewritten == {
                    start .0 * => {
                        start ::= 0 generated[3]* 6;
                        * => {
                            generated[3] ::= (1 a 2 b 3)* 5;
                        }
                    }
                }
                types == {
                    start .0: {
                        * * a: [[typeof a]],
                        * * b: [[typeof b]],
                    };
                    start .0 *: {
                        * a: [typeof a],
                        * b: [typeof b],
                    };
                    start .0 * *: {
                        a: typeof a,
                        b: typeof b,
                    };
                    start .0 * * a: typeof a;
                    start .0 * * b: typeof b;
                }
            },
        );
    }

    #[test]
    fn bound_multilevel() {
        verify(
            input!(
                start ::= (s:((a:a) (b:b)));
            ),
            quote! {
                input == {
                    start ::= s:(a:a b:b);
                }
                types == {
                    start .0: {
                        s: {
                            a: typeof a,
                            b: typeof b,
                        }
                    }
                }
                typedefs == {
                    struct Gen0 {
                        a: Terminal,
                        b: Terminal,
                    }

                    struct Gen1 {
                        s: Gen0,
                    }
                }
                typedefs_and_rules == {
                    start -> Type1 ::= s:[sym0]* => { Type1 { s } };
                    sym0 -> Type0 ::= a:a b:b => { Type0 { a, b } };
                    struct Type0 {
                        a: Terminal,
                        b: Terminal,
                    }
                    struct Type1 {
                        s: Vec<Type0>,
                    }
                }
                stage3 == {
                    #![terminals(a, b)]
                    start -> Type1 ::= bind0:sym1 => { bind0 };
                    sym1 -> Type1 ::= s:sym2 => { Type1 { s } };
                    sym2 -> Vec<Type0> ::= bind0:sym2 bind1:sym0 => { bind0.push(bind1); bind0 }
                        | [] => { Vec::new() };
                    sym0 -> Type0 ::= a:a b:b => { Type0 { a, b } };
                    struct Type0 {
                        a: Terminal,
                        b: Terminal,
                    }
                    struct Type1 {
                        s: Vec<Type0>,
                    }
                }
            },
        );
    }
}
