use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;

use crate::graph::*;
use crate::utils::ComparableTokenStream;

#[salsa::query_group(InputDatabase)]
pub trait Input {
    #[salsa::input]
    fn input_graph(&self) -> GraphSlice;

    fn pretty_input(&self) -> String;

    fn tokenize_input(&self) -> ComparableTokenStream;

    fn tokenize_input_node(&self, node: NodeRef) -> ComparableTokenStream;

    fn tokenize_children(
        &self,
        node_id: NodeId,
        separator: Option<ComparableTokenStream>,
        may_add_parentheses: bool,
    ) -> ComparableTokenStream;

    fn tokenize_each_child(&self, node_id: NodeId) -> Vec<ComparableTokenStream>;

    fn need_parentheses_around_children(&self, node_id: NodeId) -> bool;
}

fn pretty_input(db: &dyn Input) -> String {
    db.tokenize_input().to_string()
}

fn tokenize_input(db: &dyn Input) -> ComparableTokenStream {
    let mut result = TokenStream::new();
    for root_id in db.input().graph().roots() {
        result.extend(db.tokenize_input_node(root_id).token_stream());
    }
    result.into()
}

fn tokenize_input_node(db: &dyn Input, node: NodeRef) -> ComparableTokenStream {
    match node.step() {
        Step::Symbol { symbol } => {
            let ident = symbol.to_string();
            let mut result = TokenStream::new();
            result.extend(vec![TokenTree::Ident(Ident::new(
                &ident[..],
                Span::call_site(),
            ))]);
            result
        }
        Step::Sequence { min: 0, max: None } => {
            let children: TokenStream = db.tokenize_children(node, None, true).into();
            quote! { #children * }
        }
        Step::Group(GroupKind::Sum | GroupKind::Stmt) => db
            .tokenize_children(node, Some(quote! { | }.into()), false)
            .into(),
        Step::Idx(..) | Step::Group(GroupKind::Product) => {
            db.tokenize_children(node, None, false).into()
        }
        Step::Bind { label } => {
            let label_str = label.to_string();
            let bind_ident = TokenTree::Ident(Ident::new(&label_str[..], Span::call_site()));
            let children: TokenStream = db.tokenize_children(node, None, true).into();
            quote! { #bind_ident : #children }
        }
        Step::Stmt { lhs } => {
            let lhs_ident = lhs.to_string();
            let lhs = TokenTree::Ident(Ident::new(&lhs_ident[..], Span::call_site()));
            let rhs: TokenStream = db.tokenize_children(node, None, false).into();
            quote! { #lhs ::= #rhs ; }
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
    .into()
}

fn tokenize_children(
    db: &dyn Input,
    node: NodeRef,
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

fn tokenize_each_child(db: &dyn Input, node: NodeRef) -> Vec<ComparableTokenStream> {
    node.children()
        .map(|child_id| db.tokenize_node(child_id))
        .collect()
}

fn need_parentheses_around_children(db: &dyn Input, node: NodeRef) -> bool {
    node.children()
        .enumerate()
        .any(|(i, node_ref)| match (i, node_ref.step()) {
            (0, Step::Fragment(..)) => false,
            (0, Step::Sequence { .. }) => false,
            _ => true,
        })
}
