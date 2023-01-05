use std::iter;
use std::path::Path;
use std::rc::Rc;

use itertools::Itertools;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;

use crate::graph::*;
use crate::utils::ComparableTokenStream;

#[salsa::query_group(RulesDatabase)]
pub trait Rules: Input {
    fn rich_graph_with_rules(&self) -> Rc<RichGraph>;

    fn pretty_rules(&self) -> String;

    fn tokenize_rules(&self) -> ComparableTokenStream;

    fn rule_graph(&self, node_id: NodeId) -> Option<PathwayGraph>;

    // fn tokenize_node(&self, node_id: NodeId) -> ComparableTokenStream;

    // fn node(&self, node_id: NodeId) -> Option<Step>;

    // fn tokenize_children(
    //     &self,
    //     node_id: NodeId,
    //     separator: Option<ComparableTokenStream>,
    //     may_add_parentheses: bool,
    // ) -> ComparableTokenStream;

    // fn tokenize_each_child(&self, node_id: NodeId) -> Vec<ComparableTokenStream>;

    // fn need_parentheses_around_children(&self, node_id: NodeId) -> bool;

    // fn children(&self, node_id: NodeId) -> Vec<NodeId>;
}

fn rich_graph_with_rules(db: &dyn Rules) -> GraphSlice {
    let mut input: RichGraph = db.input().into();
    let mut new_graphs = vec![];
    {
        let graph = input.graph_mut();
        new_graphs.extend(graph.iter().filter_map(|node| db.rule_graph(node.id)));
    }

    input.merge_graphs(new_graph);
    Rc::new(input)
}

fn pretty_rules(db: &dyn Rules) -> String {
    db.tokenize_rules().to_string()
}

fn tokenize_rules(db: &dyn Rules) -> ComparableTokenStream {
    let graph = db.rich_graph_with_rules();
}

fn rule_graph(db: &dyn Rules, node_id: NodeId) -> Option<PathwayGraph> {
    let children: Vec<Node> = db.children(node_id);
    match &children[..] {
        &[Node {
            id: _,
            step: Step::Fragment(..),
        }] => None,
        &[Node {
            id: _,
            step: Step::Group(GroupKind::Product),
        }] => None,
        &[Node {
            id,
            step: Step::Sequence { min, max },
        }] => {
            let mut result = PathwayGraph::new();
            let fragment = result.node(Step::Fragment());
            let seq = result.node(Step::Sequence { min, max }, vec![fragment]);
            result.node(Step::Rule, vec![seq]);
            Some(result)
        }
        _ => None,
    }
}

fn rules(db: &dyn Rules) -> Rc<RichGraph> {
    db.input()
}

fn rules_for_node(db: &dyn ProvideInput, node_id: NodeId) -> BTreeMap<Path, Vec<Rule>> {}

fn tokenize_input(db: &dyn ProvideInput) -> ComparableTokenStream {
    let mut result = TokenStream::new();
    for root_id in db.input().graph().roots() {
        result.extend(db.tokenize_node(root_id).0);
    }
    result.into()
}

fn tokenize_node(db: &dyn ProvideInput, node_id: NodeId) -> ComparableTokenStream {
    match db.node(node_id).expect("invalid node_id") {
        Step::Fragment(fragment_id) => {
            let input = db.input();
            let ident = input
                .sym(fragment_id)
                .expect("incorrect id in Fragment(fragment_id)");
            let mut result = TokenStream::new();
            result.extend(vec![TokenTree::Ident(Ident::new(ident, Span::call_site()))]);
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
            let input = db.input();
            let bind = input
                .bind(bind_id)
                .expect("incorrect id in Bind { bind_id }");
            let bind_ident = TokenTree::Ident(Ident::new(bind, Span::call_site()));
            let children: TokenStream = db.tokenize_children(node_id, None, true).into();
            quote! { #bind_ident : #children }
        }
        Step::StmtFragment(fragment_id) => {
            let input = db.input();
            let lhs_ident = input
                .sym(fragment_id)
                .expect("incorrect id in StmtFragment(fragment_id)");
            let lhs = TokenTree::Ident(Ident::new(lhs_ident, Span::call_site()));
            let rhs: TokenStream = db.tokenize_children(node_id, None, false).into();
            quote! { #lhs ::= #rhs ; }
        }
        other => panic!("UNEXPECTED STEP: {:?}", other),
    }
    .into()
}

fn node(db: &dyn ProvideInput, node_id: NodeId) -> Option<Step> {
    db.input().graph().get(node_id)
}

fn tokenize_children(
    db: &dyn ProvideInput,
    node_id: NodeId,
    separator: Option<ComparableTokenStream>,
    may_add_parentheses: bool,
) -> ComparableTokenStream {
    let mut inner = TokenStream::new();

    inner.extend(
        db.tokenize_each_child(node_id)
            .into_iter()
            .map(|elem| Some(elem).into_iter())
            .intersperse(separator.into_iter())
            .filter_map(|it| it.next().map(|elem| elem.token_stream())),
    );
    let add_parentheses = db.need_parentheses_around_children(node_id) && may_add_parentheses;
    if add_parentheses {
        quote! { ( #inner ) }.into()
    } else {
        inner.into()
    }
}

fn tokenize_each_child(db: &dyn ProvideInput, node_id: NodeId) -> Vec<ComparableTokenStream> {
    db.children(node_id)
        .into_iter()
        .map(|child_id| db.tokenize_node(child_id))
        .collect()
}

fn need_parentheses_around_children(db: &dyn ProvideInput, node_id: NodeId) -> bool {
    db.children(node_id)
        .into_iter()
        .enumerate()
        .any(|(i, id)| match (i, db.node(id).unwrap()) {
            (0, Step::Fragment(..)) => false,
            (0, Step::Sequence { .. }) => false,
            _ => true,
        })
}
