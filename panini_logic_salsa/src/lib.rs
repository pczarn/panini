use std::collections::{BTreeMap, BTreeSet};
use std::collections::btree_set::Range;
use std::rc::Rc;

use elsa::FrozenIndexSet;
use quote::quote;
use proc_macro2::{TokenStream, Ident, Span};
use indexmap::IndexSet;

mod graph;

use itertools::Itertools;

#[salsa::query_group(ProvideInputStorage)]
trait ProvideInput {
    #[salsa::input]
    fn input(&self) -> Rc<Input> {
        self.input.clone()
    }

    fn pretty_input(&self) -> String;

    // TODO: Functional pretty print.

}

fn pretty_input(db: &dyn ProvideInput) -> String {
    db.input().reduce().to_string()
}

#[salsa::database(ProvideInputStorage)]
#[derive(Default)]
struct DatabaseStruct {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for DatabaseStruct {}

pub type IdentId = u32;
pub type ExprId = u32;
pub type AttrId = u32;
pub type LexerId = u32;
pub type TyId = u32;
pub type BindId = u32;
pub type FragmentId = u32;
pub type SpanId = u32;

pub const NULL_NODE: usize = 0;

pub type StepId = usize;

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
        }.fmt(fmt)
    }
}

impl Input {
    fn reduce(&self) -> TokenStream {
        let mut content = vec![None; self.graph.nodes.len()];
        for no_outgoing in self.graph.walk() {
            let token_stream = match self.graph.get(no_outgoing.id) {
                Some(Step::Fragment(fragment_id)) => {
                    let mut stream = TokenStream::new();
                    let mut nth = self.lhs_set.iter().nth(fragment_id as usize);
                    let ident = nth.as_ref().expect("incorrect id in Fragment(fragment_id)");
                    stream.extend(vec![TokenTree::Ident(Ident::new(ident, Span::call_site()))]);
                    stream
                }
                Some(Step::Sequence { min: 0, max: None }) => {
                    let mut stream = TokenStream::new();
                    let children: Vec<_> = no_outgoing.children.collect();
                    stream.extend(children.iter().cloned().map(|id| content[id as usize].clone().unwrap()));
                    let more_than_only_one_fragment_or_sequence = |(i, id)| {
                        match (i, self.graph.get(id).unwrap()) {
                            (0, Step::Fragment(..)) => false,
                            (0, Step::Sequence { .. }) => false,
                            _ => true,
                        }
                    };
                    let add_parentheses = children.into_iter().enumerate().any(more_than_only_one_fragment_or_sequence);
                    if add_parentheses {
                        quote! { ( #stream ) * }
                    } else {
                        quote! { #stream * }
                    }
                }
                Some(Step::Idx(IdxKind::Sum, _idx)) => {
                    let mut stream = TokenStream::new();
                    let alts = no_outgoing.children.map(|id| content[id as usize].clone().unwrap());
                    let pipe = quote! { | };
                    stream.extend(alts.intersperse(pipe));
                    stream
                }
                Some(Step::Idx(IdxKind::Product, _idx)) => {
                    let mut stream = TokenStream::new();
                    let children: Vec<_> = no_outgoing.children.collect();
                    stream.extend(children.iter().cloned().map(|id| content[id as usize].clone().unwrap()));
                    stream
                }
                Some(Step::Bind { bind_id, idx: _ }) => {
                    let mut nth = self.bind_set.iter().nth(bind_id as usize);
                    let ident = nth.as_ref().expect("incorrect id in Bind { bind_id }");
                    let ident = TokenTree::Ident(Ident::new(ident, Span::call_site()));
                    let mut rhs = TokenStream::new();
                    rhs.extend(no_outgoing.children.map(|id| content[id as usize].clone().unwrap()));
                    quote! { #ident : #rhs }
                }
                Some(Step::StmtFragment(fragment_id)) => {
                    let mut nth = self.lhs_set.iter().nth(fragment_id as usize);
                    let ident = nth.as_ref().expect("incorrect id in StmtFragment(fragment_id)");
                    let lhs = TokenTree::Ident(Ident::new(ident, Span::call_site()));
                    let mut rhs = TokenStream::new();
                    rhs.extend(no_outgoing.children.map(|id| content[id as usize].clone().unwrap()));
                    quote! { #lhs ::= #rhs ; }
                }
                Some(Step::Sequence { min: _, max: _ }) => panic!(),
                Some(other) => panic!("UNEXPECTED STEP: {:?}", other),
                None => panic!("err")
            };
            eprintln!("{:?}", token_stream.to_string());
            content[no_outgoing.id as usize] = Some(token_stream);
        }
        let mut result = TokenStream::new();
        for root_id in self.graph.roots() {
            result.extend(content.get(root_id).cloned().expect("err"));
        }
        result
    }
}

#[derive(Clone, Debug)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<Edge>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Edge {
    Forward {
        a: usize,
        b: usize,
    },
    Backward {
        b: usize,
        a: usize,
    },
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Idx(IdxKind, usize),
    Fragment(FragmentId),
    StmtFragment(FragmentId),
    StmtTy(TyId),
    // Class(Class, Symbol),
    // RightQuote,
    Bind { bind_id: BindId, idx: usize },
    Sequence { min: u32, max: Option<u32> },
    SequenceEnd,
    SequenceToken,
    Max,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum IdxKind {
    Sum,
    Product,
    Stmt,
}

struct NodeWithChildren<'a> {
    id: StepId,
    children: Box<dyn Iterator<Item = StepId> + 'a>,
}

impl PathwayGraph {
    pub fn new() -> Self {
        // let null_node = Node::Step { step: Step::Max, child: NULL_NODE, parent: NULL_NODE };
        // PathwayGraph { nodes: vec![null_node] }
        PathwayGraph {
            nodes: vec![],
            edges: BTreeSet::new(),
        }
    }

    pub fn node(&mut self, step: Step, children: Vec<StepId>) -> StepId {
        let idx = self.nodes.len();
        self.nodes.push(step);
        for child in children {
            self.add_edge(idx, child);
        }
        idx
    }

    fn add_edge(&mut self, parent: StepId, child: StepId) {
        self.edges.insert(Edge::Forward {
            a: parent,
            b: child,
        });
        self.edges.insert(Edge::Backward {
            b: child,
            a: parent,
        });
    }

    pub fn siblings(&self, node: StepId) -> impl Iterator<Item=StepId> + '_ {
        self.parents(node)
            .flat_map(move |parent| self.children(parent))
            .filter(move |&child| child != node)
    }

    pub fn parents(&self, child: StepId) -> impl Iterator<Item=StepId> + '_ {
        let parent_edge_a = Edge::Backward { b: child, a: 0 };
        let parent_edge_b = Edge::Backward { b: child, a: !0 };
        self.edges.range(parent_edge_a .. parent_edge_b)
            .map(|&edge| {
                if let Edge::Backward { a, .. } = edge { a } else { unreachable!() }
            })
    }

    pub fn children(&self, parent: StepId) -> impl Iterator<Item=StepId> + '_ {
        let sibling_edge_a = Edge::Forward { a: parent, b: 0 };
        let sibling_edge_b = Edge::Forward { a: parent, b: !0 };
        self.edges.range(sibling_edge_a .. sibling_edge_b)
            .map(|&edge| {
                if let Edge::Forward { b, .. } = edge { b } else { unreachable!() }
            })
    }

    pub fn iter(&self) -> impl Iterator<Item=&Step> + '_ {
        self.nodes.iter()
    }

    pub fn get(&self, step_id: StepId) -> Option<Step> {
        self.nodes.get(step_id).cloned()
    }

    pub fn roots(&self) -> impl Iterator<Item=StepId> {
        let mut is_root = vec![true; self.nodes.len()];
        for &edge in &self.edges {
            match edge {
                Edge::Forward { a, b } => {
                    is_root[b as usize] = false;
                }
                _ => {}
            }
        }
        is_root.into_iter().enumerate().filter_map(|(id, root)| if root { Some(id) } else { None })
    }

    pub fn walk<'a>(&'a self) -> impl Iterator<Item=NodeWithChildren<'a>> {
        self.iter().enumerate().map(|(i, &node)|
            NodeWithChildren { id: i as StepId, children: Box::new(self.children(i as StepId)) as Box<dyn Iterator<Item=StepId> + 'a> }
        )
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
                let children = args.into_iter().map(|(step, children_vec)| input.graph.node(step, children_vec)).collect();
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

use proc_macro2::{TokenTree, Delimiter};

#[derive(Clone, Debug)]
enum VerifyState {
    ExpectGroupName,
    ExpectEq { group_name: String },
    ExpectNextEq { group_name: String },
    ExpectBrace { group_name: String },
}

#[derive(Clone, Debug)]
struct VerifyGroup {
    name: String,
    content: TokenStream,
}

fn verify(input: Input, tokens: TokenStream) {
    let mut groups = vec![];
    let mut state = VerifyState::ExpectGroupName;
    for token_tree in tokens.into_iter() {
        match (token_tree, state.clone()) {
            (TokenTree::Ident(ident), VerifyState::ExpectGroupName) => {
                state = VerifyState::ExpectEq { group_name: ident.to_string() };
            }
            (TokenTree::Punct(punct), VerifyState::ExpectEq { group_name }) => {
                assert_eq!(punct.as_char(), '=');
                state = VerifyState::ExpectNextEq { group_name };
            }
            (TokenTree::Punct(punct), VerifyState::ExpectNextEq { group_name }) => {
                assert_eq!(punct.as_char(), '=');
                state = VerifyState::ExpectBrace { group_name };
            }
            (TokenTree::Group(group), VerifyState::ExpectBrace { group_name })  => {
                assert_eq!(group.delimiter(), Delimiter::Brace);
                groups.push(
                    VerifyGroup {
                        name: group_name,
                        content: group.stream(),
                    }
                );
                state = VerifyState::ExpectGroupName;
            }
            (token_tree, state) => {
                panic!("unexpected token tree {:?}, current state {:?}", token_tree, state);
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
            quote!{
                input == {
                    start ::= a b;
                }
                flattened == {
                    start@0 a@0;
                    start@0 b@1;
                }
                trace == {
                    start@0 a@0 => a;
                    start@0 b@1 => b; 
                }
                rewritten == {
                    start ::= 0 a 1 b 2;
                }
                types == {
                    start: (typeof a, typeof b);
                }
            }
        );
    }

    #[test]
    fn multilevel_sequence() {
        verify(
            input!(
                start ::= ((((a:a) (b:b))*)*);
            ),
            quote!{
                input == {
                    start ::= (a:a b:b)**;
                }
                flattened == {
                    start@0 a@0;
                    start@0 b@1;
                }
                trace == {
                    // 0
                    start@0 * * => LParen;
                    // 1
                    start@0 * * a@0 => a;
                    // 2
                    start@0 * * b@1 => b;
                    // 3
                    start@0 * * SequenceEnd => RParen;
                    // 4
                    start@0 * * SequenceToken => *;
                    // 5
                    start@0 * SequenceToken => *;
                    // 6
                }
                rewritten == {
                    start@0 * => {
                        start ::= 0 generated[3]* 6;
                    }
                    start@0 * * => {
                        generated[3] ::= (1 a 2 b 3)* 5;
                    }
                }
                types == {
                    start@0: {
                        * * a: [[typeof a]],
                        * * b: [[typeof b]]
                    };
                    start@0 *: {
                        * a: [typeof a],
                        * b: [typeof b]
                    };
                    start@0 * *: {
                        a: typeof a,
                        b: typeof b
                    };
                    start@0 * * a: typeof a;
                    start@0 * * b: typeof b;
                }
            }
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
                    start@0: {
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
            }
        );
    }
}
