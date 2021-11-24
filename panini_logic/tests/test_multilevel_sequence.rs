extern crate cfg;
extern crate gearley;
extern crate panini_logic;
#[macro_use]
extern crate maplit;

mod support;

use cfg::Cfg;

use panini_logic::input::{Stmts, Stmt, RhsAst, Rhs, RhsElement, Action, Sequence};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::middle::flatten_stmts::{FlattenStmts, Path, Position};
use panini_logic::middle::trace::{Trace, TraceToken};
use panini_logic::middle::rule_rewrite::{RuleRewrite, InputSymbol, InputRule};
use panini_logic::middle::type_collector::{TypeCollector, Type};

#[test]
fn test_multilevel_sequence() {
    let (start, a, b) = (0, 1, 2);
    let (bind_a, bind_b) = (0, 1);

    let graph = pathway_graph![
        Step::IdxWithFragment { // 0
            idx: 0,
            fragment: start,
        },
        Step::Sequence { min: 0, max: None }, // 1
        Step::Sequence { min: 0, max: None }, // 2
        [
            Step::Bind(bind_a), // 3
            Step::IdxWithFragment { // 4
                idx: 0,
                fragment: a,
            },
        ],
        [
            Step::Bind(bind_b), // 5
            Step::IdxWithFragment { // 6
                idx: 1,
                fragment: b,
            },
        ],
    ];
    assert_eq!(flatten.paths, expected_paths);

    // Trace
    let mut trace = Trace::from_stmts(&stmts);
    let expected_tokens = btreemap! {
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::Sequence { min: 0, max: None }
            ]
        } => TraceToken::LParen,
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::Sequence { min: 0, max: None },
                Step::IdxWithFragment { idx: 0, fragment: a }
            ]
        } => TraceToken::Fragment(a),
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::Sequence { min: 0, max: None },
                Step::IdxWithFragment { idx: 1, fragment: b }
            ]
        } => TraceToken::Fragment(b),
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::Sequence { min: 0, max: None },
                Step::SequenceEnd
            ]
        } => TraceToken::RParen,
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::Sequence { min: 0, max: None },
                Step::SequenceToken
            ]
        } => TraceToken::Star,
        Path {
            steps: vec![
                Step::IdxWithFragment { idx: 0, fragment: start },
                Step::Sequence { min: 0, max: None },
                Step::SequenceToken
            ]
        } => TraceToken::Star,
        // Path {
        //     position: vec![
        //         Position::IdxWithFragment { idx: 1, fragment: 1 },
        //         Position::Alternative(0),
        //         Position::Idx(0),
        //         Position::Sequence { min: 0, max: None },
        //         Position::SequenceToken
        //     ]
        // } => TraceToken::Star,
    };
    assert_eq!(trace.tokens(), &expected_tokens);

    // Rule rewrite
    let mut cfg: Cfg = Cfg::new();
    let (_start_sym, _a_sym, _b_sym) = cfg.sym();
    let mut rewrite = RuleRewrite::new(&mut trace);
    rewrite.rewrite(expected_paths);
    let g3_path = Path {
        position: vec![
            Position::IdxWithFragment { idx: 0, fragment: start },
            Position::Sequence { min: 0, max: None },
        ]
    };
    let g3 = InputSymbol::FromPath(g3_path.clone());
    let expected_rules = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
            ]
        } => InputRule {
            lhs: InputSymbol::Fragment(start),
            rhs: btreemap! {
                0 => g3.clone()
            },
            sequence: Some((0, None)),
            traces: btreemap! { Some(0) => 0, None => 6 },
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
            ]
        } => InputRule {
            lhs: g3,
            rhs: btreemap! { 0 => InputSymbol::Fragment(a), 1 => InputSymbol::Fragment(b) },
            sequence: Some((0, None)),
            traces: btreemap! { Some(0) => 1, Some(1) => 2, Some(2) => 3, None => 5 },
        }
    };
    assert_eq!(rewrite.rules, expected_rules);
    let expected_symbols = btreeset! { g3_path };
    assert_eq!(rewrite.new_symbols, expected_symbols);

    // Types
    let expected_types = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 }
            ]
        } => btreeset! {
            Type::Struct {
                fields: btreemap! {
                    Path {
                        position: vec![
                            Position::Sequence { min: 0, max: None },
                            Position::Sequence { min: 0, max: None },
                            Position::Bind(0)
                        ]
                    } => Type::Sequence {
                        ty: Box::new(Type::Sequence {
                            ty: Box::new(Type::TypeOfFragment { fragment: 1 })
                        })
                    },
                    Path {
                        position: vec![
                            Position::Sequence { min: 0, max: None },
                            Position::Sequence { min: 0, max: None },
                            Position::Bind(1)
                        ]
                    } => Type::Sequence {
                        ty: Box::new(Type::Sequence {
                            ty: Box::new(Type::TypeOfFragment { fragment: 2 })
                        })
                    }
                }
            }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Sequence { min: 0, max: None }
            ]
        } => btreeset! {
            Type::Struct {
                fields: btreemap! {
                    Path {
                        position: vec![
                            Position::Sequence { min: 0, max: None },
                            Position::Bind(0)
                        ]
                    } => Type::Sequence {
                        ty: Box::new(Type::TypeOfFragment { fragment: 1 })
                    },
                    Path {
                        position: vec![
                            Position::Sequence { min: 0, max: None },
                            Position::Bind(1)
                        ]
                    } => Type::Sequence {
                        ty: Box::new(Type::TypeOfFragment { fragment: 2 })
                    }
                }
            }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None }
            ]
        } => btreeset! {
            Type::Struct {
                fields: btreemap! {
                    Path {
                        position: vec![Position::Bind(0)]
                    } => Type::TypeOfFragment { fragment: 1 },
                    Path {
                        position: vec![Position::Bind(1)]
                    } => Type::TypeOfFragment { fragment: 2 }
                }
            }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::Bind(0)
            ]
        } => btreeset! {
            Type::TypeOfFragment { fragment: 1 }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::Bind(1)
            ]
        } => btreeset! {
            Type::TypeOfFragment { fragment: 2 }
        }
    };
    let mut collector = TypeCollector::new();
    collector.collect(flatten.paths);
    assert_eq!(collector.types, expected_types);
}
