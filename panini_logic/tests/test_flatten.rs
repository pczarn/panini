extern crate cfg;
extern crate gearley;
extern crate panini_logic;
#[macro_use]
extern crate maplit;

mod support;

use cfg::Cfg;

use panini_logic::input::{PathwayGraph, Node, Step, NULL_NODE};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::middle::flatten_stmts::{FlattenStmts, Path, Position};
use panini_logic::middle::trace::{Trace, TraceToken};
use panini_logic::middle::rule_rewrite::{RuleRewrite, InputSymbol, InputRule};
use panini_logic::middle::type_collector::{TypeCollector, Type};

#[test]
fn test_flatten() {
    let lower = grammar_make_lower! {
        sub enum_stream -> (()) {}
        start -> (()) ::= (a b) => { () };
        a -> (()) ::= (b*) => { () };
    };
    // fragments: BTreeMap<String, FragmentId>,
    // fragment_vec: Vec<String>,
    // stmt_map: BTreeMap<(FragmentId, usize), ExprId>,

    let a_frag: FragmentId = 0;
    let b_frag: FragmentId = 1;
    let start_frag: FragmentId = 2;

    let expected_pathway_graph = PathwayGraph::new();

    assert_eq!(lower.tables.action_vec.len(), 2);
    assert_eq!(lower.pathway_graph, expected_pathway_graph);

    // let (start, a, b) = (0, 1, 2);
    // let stmts = Stmts {
    //     attr_arguments: AttrArguments {
    //         lexer_arguments: None,
    //     },
    //     stmts: vec![
    //         // start ::= a b;
    //         Stmt {
    //             lhs: start,
    //             body: vec![
    //                 (
    //                     0,
    //                     Rhs(vec![
    //                         RhsElement {
    //                             bind: None,
    //                             elem: RhsAst::Fragment(a),
    //                         },
    //                         RhsElement {
    //                             bind: None,
    //                             elem: RhsAst::Fragment(b),
    //                         }
    //                     ]),
    //                     Action {
    //                         expr: None,
    //                     }
    //                 )
    //             ],
    //             ty: None,
    //         },
    //         // a ::= b*;
    //         Stmt {
    //             lhs: a,
    //             body: vec![
    //                 (
    //                     0,
    //                     Rhs(vec![
    //                         RhsElement {
    //                             bind: None,
    //                             elem: RhsAst::Sequence(Sequence {
    //                                 rhs: Rhs(vec![
    //                                     RhsElement {
    //                                         bind: None,
    //                                         elem: RhsAst::Fragment(b),
    //                                     }
    //                                 ]),
    //                                 min: 0,
    //                                 max: None,
    //                             }),
    //                         }
    //                     ]),
    //                     Action {
    //                         expr: None,
    //                     },
    //                 )
    //             ],
    //             ty: None,
    //         },
    //     ],
    //     lexer: None,
    // };
    // let mut flatten = FlattenStmts::new();
    // flatten.flatten_stmts(&stmts);

    // let expected_paths = vec![
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment {
    //                 idx: 0,
    //                 fragment: start,
    //             },
    //             Position::IdxWithFragment {
    //                 idx: 0,
    //                 fragment: a,
    //             },
    //         ]
    //     },
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment {
    //                 idx: 0,
    //                 fragment: start,
    //             },
    //             Position::IdxWithFragment {
    //                 idx: 1,
    //                 fragment: b,
    //             },
    //         ]
    //     },
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment {
    //                 idx: 1,
    //                 fragment: a,
    //             },
    //             Position::Sequence { min: 0, max: None },
    //             Position::IdxWithFragment {
    //                 idx: 0,
    //                 fragment: b,
    //             }
    //         ]
    //     },
    // ];
    // assert_eq!(flatten.paths, expected_paths);

    // // Trace
    // let mut trace = Trace::from_stmts(&stmts);
    // let expected_tokens = btreemap! {
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 0, fragment: 0 },
    //             Position::IdxWithFragment { idx: 0, fragment: 1 }
    //         ]
    //     } => TraceToken::Fragment(1),
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 0, fragment: 0 },
    //             Position::IdxWithFragment { idx: 1, fragment: 2 }
    //         ]
    //     } => TraceToken::Fragment(2),
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 1, fragment: 1 },
    //             Position::Sequence { min: 0, max: None },
    //             Position::IdxWithFragment { idx: 0, fragment: 2 }
    //         ]
    //     } => TraceToken::Fragment(2),
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 1, fragment: 1 },
    //             Position::Sequence { min: 0, max: None },
    //             Position::SequenceToken
    //         ]
    //     } => TraceToken::Star,
    // };
    // assert_eq!(trace.tokens(), &expected_tokens);

    // // Rule rewrite
    // let mut cfg: Cfg = Cfg::new();
    // let (_start_sym, _a_sym, _b_sym) = cfg.sym();
    // let mut rewrite = RuleRewrite::new(&mut trace);
    // rewrite.rewrite(expected_paths);
    // let expected_rules = btreemap! {
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 0, fragment: start },
    //         ]
    //     } => InputRule {
    //         lhs: InputSymbol::Fragment(start),
    //         rhs: btreemap! { 0 => InputSymbol::Fragment(a), 1 => InputSymbol::Fragment(b) },
    //         sequence: None,
    //         traces: btreemap! { Some(0) => 0, Some(1) => 1, Some(2) => 2 },
    //     },
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 1, fragment: a },
    //             Position::Sequence { min: 0, max: None }
    //         ]
    //     } => InputRule {
    //         lhs: InputSymbol::Fragment(a),
    //         rhs: btreemap! { 0 => InputSymbol::Fragment(b) },
    //         sequence: Some((0, None)),
    //         traces: btreemap! { Some(0) => 2, Some(1) => 3, None => 4 },
    //     }
    // };
    // assert_eq!(rewrite.rules, expected_rules);

    // // Types
    // let expected_types = btreemap! {
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 0, fragment: 0 }
    //         ]
    //     } => btreeset! {
    //         Type::Tuple {
    //             fields: btreemap! {
    //                 0 => Type::TypeOfFragment { fragment: 1 },
    //                 1 => Type::TypeOfFragment { fragment: 2 },
    //             }
    //         }
    //     },
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 1, fragment: 1 }
    //         ]
    //     } => btreeset! {
    //         Type::Sequence {
    //             ty: Box::new(Type::TypeOfFragment { fragment: 2 })
    //         }
    //     },
    //     Path {
    //         position: vec![
    //             Position::IdxWithFragment { idx: 1, fragment: 1 },
    //             Position::Sequence { min: 0, max: None },
    //         ]
    //     } => btreeset! {
    //         Type::TypeOfFragment { fragment: 2 }
    //     }
    // };
    // let mut collector = TypeCollector::new();
    // collector.collect(flatten.paths);
    // assert_eq!(collector.types, expected_types);
}
