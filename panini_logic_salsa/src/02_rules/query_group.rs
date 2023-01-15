use std::iter;
use std::path::Path;
use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;

use crate::graph::*;
use crate::utils::ComparableTokenStream;
use crate::input::Input;
use crate::values::*;

#[salsa::query_group(RulesStorage)]
pub trait Rules: Input {
    fn rule_graph(&self) -> PathwayGraph;

    fn pretty_rules(&self) -> String;

    fn tokenize_rules(&self) -> ComparableTokenStream;

    fn tokenize_rules_node(&self, node: NodeHash) -> ComparableTokenStream;

    fn rule_graph_for_node(&self, node: NodeHash) -> Option<PathwayGraph>;

    fn child_steps(&self, node: NodeHash) -> Vec<Step>;

    fn symbol_for_node(&self, node: NodeHash) -> Symbol;
}

fn rule_graph(db: &dyn Rules) -> PathwayGraph {
    let mut result = db.input_graph();
    let subgraphs: Vec<PathwayGraph> = result.iter().filter_map(|node| db.rule_graph_for_node(node)).collect();
    result.join(&subgraphs[..]);
    result
}

fn pretty_rules(db: &dyn Rules) -> String {
    db.tokenize_rules().to_string()
}

fn tokenize_rules(db: &dyn Rules) -> ComparableTokenStream {
    let graph = db.rule_graph();
    let mut result = TokenStream::new();
    PATHWAY_GRAPH.with(|g| {
        for root in g.roots() {
            result.extend(db.tokenize_rules_node(root).token_stream());
        }
    })
    result.into()
}

fn rule_graph_for_node(db: &dyn Rules, node: NodeHash) -> Option<PathwayGraph> {
    let children = db.child_steps(node);
    match &children[..] {
        &[Step::Symbol { .. }] => None,
        &[Step::Group(GroupKind::Product)] => None,
        &[Step::Sequence { range }] => {
            let lhs = db.symbol_for_node(node);
            Some(graph! {
                (External { node })
                --
                (Rule { lhs })
                --
                (Sequence { range })
            })
        }
        _ => None,
    }
}

fn child_steps(db: &dyn Rules, node: NodeHash) -> Vec<Step> {
    node.children().map(|child| child.step()).collect()
}

struct MutlipleTokenStreams

fn tokenize_rules_node(db: &dyn Rules, node: NodeHash) -> Vec<ComparableTokenStream> {
    match node.step() {
        Step::Rule { lhs } => {
            let children: TokenStream = db.tokenize_children(node, None, true).into();
            let lhs = db.lookup_symbol(lhs).to_token_stream();
            quote! { => Rule { $lhs ::= $children } }
        }
        Step::Symbol { symbol } => {
            db.lookup_symbol(symbol).to_token_stream()
        }
        // TODO other ranges
        Step::Sequence { range: SequenceRange { min: 0, max: None } } => {
            let children: MutlipleTokenStreams = db.tokenize_children(node, None, true).into();
            children.prepend(quote! { * })
        }
        Step::Group(GroupKind::Sum | GroupKind::Stmt) =>
            // split into multiple
        db
            .tokenize_children(node, None, false)
            .into(),
        Step::Idx(..) | Step::Group(GroupKind::Product) => {
            db.tokenize_children(node, None, false).into()
        }
        Step::Bind { label } => {
            let bind_ident = db.lookup_label(label).to_token_stream();
            let children: TokenStream = db.tokenize_children(node, None, true).into();
            quote! { #bind_ident : #children }
        }
        Step::Stmt { lhs } => {
            let lhs = db.lookup_symbol(lhs).to_token_stream();
            let rhs: TokenStream = db.tokenize_children(node, None, false).into();
            quote! { #lhs ::= #rhs ; }
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
    .into()
}

fn symbol_for_node(db: &dyn Rules, node: NodeHash) -> Symbol {
    match node.step() {
        Step::Symbol { symbol } => {
            symbol
        }
        // TODO other ranges
        Step::Sequence { range: SequenceRange { min: 0, max: None } } => {
            self.symbol(Box::new(symbol from path (node)))
        }
        Step::Group(GroupKind::Sum | GroupKind::Stmt) => {
            db.symbol_for_node(node.parents().next().unwrap())
        }
        Step::Idx(..) | Step::Group(GroupKind::Product) => {
            db.symbol_for_node(node.parents().next().unwrap())
        }
        Step::Bind { label } => {
            self.symbol(Box::new(symbol from path (node)))
            sure that for node?
        }
        Step::Stmt { lhs } => {
            lhs
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
}