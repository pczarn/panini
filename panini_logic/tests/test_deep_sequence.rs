extern crate cfg;
extern crate gearley;
extern crate panini_logic;
#[macro_use]
extern crate maplit;

mod support;

use cfg::{Cfg, ContextFree, ContextFreeRef};

use panini_logic::input::{Stmts, Stmt, RhsAst, Rhs, RhsElement, Action, Sequence};
use panini_logic::input::attr_arguments::AttrArguments;
use panini_logic::middle::ir::Ir;
use panini_logic::middle::flatten_stmts::{FlattenStmts, Path, Position};
use panini_logic::middle::trace::{Trace, TraceToken};
use panini_logic::middle::rule_rewrite::{RuleRewrite, Sym, RuleValue};
use panini_logic::middle::type_collector::{TypeCollector, Type};
use panini_logic::output::translator::IrTranslator;
use panini_logic::output::instruction::{Instruction, InstructionList};

#[test]
fn test_deep_sequence() {
    let (start, a, b) = (0, 1, 2);
    let (bind_a, bind_b) = (0, 1);
    let stmts = Stmts {
        attr_arguments: AttrArguments {
            lexer_arguments: None,
        },
        stmts: vec![
            // start ::= (a:a b:b)*;
            Stmt {
                lhs: start,
                body: vec![
                    (
                        0,
                        Rhs(vec![
                            RhsElement {
                                bind: None,
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
        Path {
            position: vec![
                Position::IdxWithFragment {
                    idx: 0,
                    fragment: start,
                },
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
    let mut rewrite = RuleRewrite::new(&mut trace);
    rewrite.rewrite(expected_paths);
    let expected_rules = btreemap! {
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: start },
                Position::Sequence { min: 0, max: None }
            ]
        } => RuleValue {
            lhs: Sym::Fragment(start),
            rhs: btreemap! { 0 => Sym::Fragment(a), 1 => Sym::Fragment(b) },
            sequence: Some((0, None)),
            traces: btreemap! {
                Some(0) => 1, Some(1) => 2, Some(2) => 3, None => 5,
            }
        },
    };
    assert_eq!(rewrite.rules, expected_rules);

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
                Position::Bind(0)
            ]
        } => btreeset! {
            Type::TypeOfFragment { fragment: 1 }
        },
        Path {
            position: vec![
                Position::IdxWithFragment { idx: 0, fragment: 0 },
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

    let ir = Ir::transform(stmts).expect("err");
    let mut cfg: Cfg = Cfg::new();
    let (g0, a_sym, b_sym, start_sym, g4) = cfg.sym();
    cfg.rule(g0).rhs([a_sym, b_sym]);
    cfg.rule(start_sym).rhs([g4]);
    cfg.rule(g4).rhs([g0]);
    cfg.rule(g4).rhs([g4, g0]);
    assert!(ir.trivial_derivation);
    support::assert_eq_rules(ir.grammar.rules(), cfg.rules());
    support::assert_eq_origins(ir.grammar.rules(), vec![Some(0), Some(1), None, None]);
    let mut nulling_cfg: Cfg = Cfg::new();
    nulling_cfg.rule(start_sym).rhs([]);
    support::assert_eq_rules(ir.nulling_grammar.rules(), nulling_cfg.rules());
    support::assert_eq_origins(ir.nulling_grammar.rules(), vec![Some(1)]);
    let expected_rule_tokens = vec![
        btreemap! {
            Some(0) => 1,
            Some(1) => 2,
            Some(2) => 3,
            None => 5,
        },
        btreemap! {}
    ];
    assert_eq!(ir.trace.rule_tokens, expected_rule_tokens);
    let (external_a_sym, external_b_sym) = (b_sym, start_sym);
    assert_eq!(ir.externalize(a_sym), external_a_sym);
    assert_eq!(ir.externalize(b_sym), external_b_sym);
    let mut translator = IrTranslator::new(ir);
    let instruction_list = translator.generate();
    let expected_instruction_list = InstructionList {
        list: vec![
            Instruction::MakeTerminalAccessorFn { terminal: external_a_sym },
            Instruction::MakeTerminalAccessorFn { terminal: external_b_sym },
            Instruction::MakeTerminalAccessorStruct { number: 2 },
        ]
    };
    assert_eq!(instruction_list, expected_instruction_list);
}
