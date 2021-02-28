extern crate cfg;
extern crate gearley;
#[macro_use]
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
fn test_bound_deep_sequence() {
    let (start, a, b) = (0, 1, 2);
    let (bind_s, bind_a, bind_b) = (0, 1, 2);
    let stmts = Stmts {
        attr_arguments: AttrArguments {
            lexer_arguments: None,
        },
        stmts: vec![
            // start ::= s:(a:a b:b)*;
            Stmt {
                lhs: start,
                body: vec![
                    (
                        0,
                        Rhs(vec![
                            RhsElement {
                                bind: Some(bind_s),
                                elem: RhsAst::Sequence(Sequence {
                                    rhs: Rhs(
                                        vec![
                                            RhsElement {
                                                bind: Some(bind_a),
                                                elem: RhsAst::Fragment(a),
                                            },
                                            RhsElement {
                                                bind: Some(bind_b),
                                                elem: RhsAst::Fragment(b),
                                            }
                                        ]
                                    ),
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
        path![
            Position::StmtFragment(start),
            Position::StmtIdx(0),
            Position::Bind(bind_s),
            Position::Sequence { min: 0, max: None },
            Position::Bind(bind_a),
            Position::Fragment(a),
        ],
        path![
            Position::StmtFragment(start),
            Position::StmtIdx(0),
            Position::Bind(bind_s),
            Position::Sequence { min: 0, max: None },
            Position::Bind(bind_b),
            Position::Fragment(b),
        ],
    ];
    assert_eq!(flatten.paths, expected_paths);

    // Trace
    // {Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Sequence { min: 0, max: None }] }: LParen, Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Sequence { min: 0, max: None }, IdxWithFragment { idx: 0, fragment: 1 }] }: Fragment(1), Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Sequence { min: 0, max: None }, IdxWithFragment { idx: 1, fragment: 2 }] }: Fragment(2), Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Sequence { min: 0, max: None }, SequenceEnd] }: RParen, Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Sequence { min: 0, max: None }, SequenceToken] }: Star
    // {Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Bind(0), Sequence { min: 0, max: None }] }: LParen, Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Bind(0), Sequence { min: 0, max: None }, Bind(1), IdxWithFragment { idx: 0, fragment: 1 }] }: Fragment(1), Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Bind(0), Sequence { min: 0, max: None }, Bind(2), IdxWithFragment { idx: 1, fragment: 2 }] }: Fragment(2), Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Bind(0), Sequence { min: 0, max: None }, SequenceEnd] }: RParen, Path { position: [IdxWithFragment { idx: 0, fragment: 0 }, Bind(0), Sequence { min: 0, max: None }, SequenceToken] }: Star}
    let mut trace = Trace::from_stmts(&stmts);
    let expected_tokens = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None }
            ]
        } => TraceToken::LParen,
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::IdxWithFragment { idx: 0, fragment: a }
            ]
        } => TraceToken::Fragment(a),
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::IdxWithFragment { idx: 1, fragment: b }
            ]
        } => TraceToken::Fragment(b),
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None },
                Position::SequenceEnd
            ]
        } => TraceToken::RParen,
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
    let (_start_sym, _a_sym, _b_sym, g3, g4) = cfg.sym();
    let mut rewrite = RuleRewrite::new(&mut trace);
    rewrite.rewrite(expected_paths);
    // rewrite.simplify_rules();
    let expected_rules = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Sequence { min: 0, max: None }
            ]
        } => InputRule {
            lhs: InputSymbol::Fragment(start),
            rhs: btreemap! { 0 => InputSymbol::Fragment(a), 1 => InputSymbol::Fragment(b) },
            sequence: Some((0, None)),
            traces: btreemap! { Some(0) => 1, Some(1) => 2, Some(2) => 3, None => 5 }
        },
    };
    assert_eq!(rewrite.rules, expected_rules);

    // Types
    let expected_types = btreemap! {
        path![Position::IdxWithFragment { idx: 0, fragment: 0 }] => Type::Struct {
            fields: btreemap! {
                path![Position::Bind(0)] => Type::Sequence {
                    ty: Box::new(Type::Struct {
                        fields: btreemap! {
                            path![Position::Bind(1)] => Type::TypeOfFragment { fragment: a },
                            path![Position::Bind(2)] => Type::TypeOfFragment { fragment: b },
                        }
                    })
                }
            }
        },
        path![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Bind(0)
            ]
        => Type::Sequence {
            ty: Box::new(Type::Struct {
                fields: btreemap! {
                    Path {
                        position: vec![
                            Position::Bind(1),
                        ]
                    } => Type::TypeOfFragment { fragment: a },
                    Path {
                        position: vec![
                            Position::Bind(2),
                        ]
                    } => Type::TypeOfFragment { fragment: b }
                }
            })
        },
            // Type::Struct {
            //     fields: btreemap! {
            //         Path {
            //             position: vec![
            //                 Position::Sequence { min: 0, max: None },
            //                 Position::Bind(1)
            //             ]
            //         } => Type::Sequence {
            //             ty: Box::new(Type::TypeOfFragment { fragment: a })
            //         },
            //         Path {
            //             position: vec![
            //                 Position::Sequence { min: 0, max: None },
            //                 Position::Bind(2)
            //             ]
            //         } => Type::Sequence {
            //             ty: Box::new(Type::Tuple {
            //                 fields: btreemap! {
            //                     1 => Type::TypeOfFragment { fragment: 2 }
            //                 }
            //             })
            //         }
            //     }
            // }
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Bind(0),
                Position::Sequence { min: 0, max: None }
            ]
        } => Type::Struct {
            fields: btreemap! {
                Path {
                    position: vec![Position::Bind(1)]
                } => Type::TypeOfFragment { fragment: 1 },
                Path {
                    position: vec![Position::Bind(2)]
                } => Type::TypeOfFragment { fragment: 2 }
            }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Bind(0),
                Position::Sequence { min: 0, max: None },
                Position::Bind(1)
            ]
        } => Type::TypeOfFragment { fragment: 1 },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
                Position::Bind(0),
                Position::Sequence { min: 0, max: None },
                Position::Bind(2)
            ]
        } => Type::TypeOfFragment { fragment: 2 },
    };
    let mut collector = TypeCollector::new();
    collector.collect(flatten.paths);
    assert_eq!(collector.final_types, expected_types);
}
