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
use panini_logic::middle::rule_rewrite::{RuleRewrite, Sym, RuleValue};
use panini_logic::middle::type_collector::{TypeCollector, Type};

#[test]
fn test_multilevel_sequence() {
    let (start, a, b) = (0, 1, 2);
    let (bind_a, bind_b) = (0, 1);
    let stmts = Stmts {
        attr_arguments: AttrArguments {
            lexer_arguments: None,
        },
        stmts: vec![
            // start ::= (a:a b:b)**;
            Stmt {
                lhs: start,
                body: vec![
                    (
                        0,
                        Rhs(vec![
                            RhsElement {
                                bind: None,
                                elem: RhsAst::Sequence(Sequence {
                                    rhs: Rhs(vec![
                                        RhsElement {
                                            bind: None,
                                            elem: RhsAst::Sequence(Sequence {
                                                rhs: Rhs(vec![
                                                    RhsElement {
                                                        bind: Some(bind_a),
                                                        elem: RhsAst::Fragment(a),
                                                    },
                                                    RhsElement {
                                                        bind: Some(bind_b),
                                                        elem: RhsAst::Fragment(b),
                                                    }
                                                ]),
                                                min: 0,
                                                max: None,
                                            })
                                        }
                                    ]),
                                    min: 0,
                                    max: None,
                                })
                            }
                        ]),
                        Action {
                            expr: None,
                        }
                    )
                ],
                ty: None,
            },
        ],
        lexer: None,
    };
    let mut flatten = FlattenStmts::new();
    flatten.flatten_stmts(&stmts);

    let expected_paths = vec![
        Path {
            position: vec![
                Position::IdxWithFragment {
                    idx: 0,
                    fragment: start,
                },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::Bind(bind_a),
                Position::IdxWithFragment {
                    idx: 0,
                    fragment: a,
                },
            ]
        },
        Path {
            position: vec![
                Position::IdxWithFragment {
                    idx: 0,
                    fragment: start,
                },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::Bind(bind_b),
                Position::IdxWithFragment {
                    idx: 1,
                    fragment: b,
                },
            ]
        },
    ];
    assert_eq!(flatten.paths, expected_paths);

    // Trace
    let mut trace = Trace::from_stmts(&stmts);
    let expected_tokens = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None }
            ]
        } => TraceToken::LParen,
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::IdxWithFragment { idx: 0, fragment: a }
            ]
        } => TraceToken::Fragment(a),
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::IdxWithFragment { idx: 1, fragment: b }
            ]
        } => TraceToken::Fragment(b),
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::SequenceEnd
            ]
        } => TraceToken::RParen,
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::Sequence { min: 0, max: None },
                Position::SequenceToken
            ]
        } => TraceToken::Star,
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::SequenceToken
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
    let g3 = Sym::FromPath(g3_path.clone());
    let expected_rules = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
            ]
        } => RuleValue {
            lhs: Sym::Fragment(start),
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
        } => RuleValue {
            lhs: g3,
            rhs: btreemap! { 0 => Sym::Fragment(a), 1 => Sym::Fragment(b) },
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
    collector.simplify_tuples();
    assert_eq!(collector.types, expected_types);
}
