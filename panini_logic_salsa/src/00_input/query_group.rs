use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;

use crate::graph::*;
use crate::values::*;
use crate::utils::ComparableTokenStream;

#[salsa::query_group(InputDatabase)]
pub trait Input {
    #[salsa::input]
    fn input_graph(&self) -> PathwayGraph;

    fn pretty_input(&self) -> String;

    fn tokenize_input(&self) -> ComparableTokenStream;

    fn tokenize_input_node(&self, node: NodeHash) -> ComparableTokenStream;

    fn tokenize_children(
        &self,
        node: NodeHash,
        separator: Option<ComparableTokenStream>,
        may_add_parentheses: bool,
    ) -> ComparableTokenStream;

    fn tokenize_each_child(&self, node: NodeHash) -> Vec<ComparableTokenStream>;

    fn need_parentheses_around_children(&self, node: NodeHash) -> bool;

    #[salsa::interned]
    fn symbol(&self, symbol: Box<dyn SymbolData>) -> Symbol;

    #[salsa::interned]
    fn label(&self, symbol: Box<dyn LabelData>) -> Label;

    #[salsa::interned]
    fn ty(&self, symbol: Box<dyn TypeData>) -> Type;
}

fn pretty_input(db: &dyn Input) -> String {
    db.tokenize_input().root().children().find map(|child| child matches Step::Tokens { tokens }).to_string()
}

fn tokenize_input(db: &dyn Input) -> PathwayGraph {
    add root node to PathwayGraph
    db.input_graph().join(|node| db.token_node_for_input_node(node))
}

fn input_node_tokens(db: &dyn Input, node: NodeHash) -> ComparableTokenStream {
    match node.step() {
        Step::Symbol { symbol } => {
            db.lookup_symbol(symbol).to_token_stream()
        }
        // TODO other ranges
        Step::Sequence { range: SequenceRange { min: 0, max: None } } => {
            quote! { CHILDREN * }
        }
        Step::Group(GroupKind::Sum | GroupKind::Stmt) => {
            quote! { CHILD | CHILD }
        }
        Step::Idx(..) | Step::Group(GroupKind::Product) => {
            quote! { CHILDREN }
        }
        Step::Bind { label } => {
            let bind_ident = db.lookup_label(label).to_token_stream();
            quote! { #bind_ident : CHILDREN }
        }
        Step::Stmt { lhs } => {
            let lhs = db.lookup_symbol(lhs).to_token_stream();
            quote! { #lhs ::= CHILDREN ; }
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
    .into()
}

fn token_node_for_input_node(db: &dyn Input, node: NodeHash) -> PathwayGraph {
    let tokens = db.input_node_tokens(node);
    check for separator and CHILD/CHILDREN, check for parenthesizing (parenthesize CHILDREN) - how to make this clear???
    let children = db.tokenize_children(node);
    replace CHILDREN with tokenized children
    or replace CHILD separator CHILD with tokenized children
    graph! {
        (External { node })
        --
        (Tokens { result })
    }
}

fn tokenize_children(
    db: &dyn Input,
    node: NodeHash,
    separator: Option<ComparableTokenStream>,
    may_add_parentheses: bool,
) -> ComparableTokenStream {
    let mut inner = TokenStream::new();

    let into_option_iter = |elem: ComparableTokenStream| Some(elem).into_iter();
    let to_token_stream = |elem: ComparableTokenStream| elem.token_stream();
    let next = |it: std::option::IntoIter<ComparableTokenStream>| it.next();

    inner.extend(
        db.tokenize_each_child(node)
            .into_iter()
            .map(into_option_iter)
            .intersperse(separator.into_iter())
            .filter_map(next)
            .map(to_token_stream),
    );
    let add_parentheses = db.need_parentheses_around_children(node) && may_add_parentheses;
    if add_parentheses {
        quote! { ( #inner ) }.into()
    } else {
        inner.into()
    }
}

fn tokenize_each_child(db: &dyn Input, node: NodeHash) -> Vec<ComparableTokenStream> {
    node.node_ref().children()
        .map(|child_id| db.tokenize_input_node(child_id))
        .collect()
}

fn need_parentheses_around_children(db: &dyn Input, node: NodeHash) -> bool {
    node.children()
        .enumerate()
        .any(|(i, node_ref)| match (i, node_ref.step()) {
            (0, Step::Symbol { .. }) => false,
            (0, Step::Sequence { .. }) => false,
            _ => true,
        })
}
