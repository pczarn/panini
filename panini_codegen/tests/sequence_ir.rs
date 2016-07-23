#![feature(rustc_private)]

extern crate cfg;
extern crate gearley;
extern crate panini_codegen;
#[macro_use]
extern crate maplit;

mod support;

use cfg::ContextFreeRef;
use gearley::grammar::Grammar;

use panini_codegen::front::ast::{self, Stmts, Stmt, RhsElement, Rhs, RhsAst, Sequence};
use panini_codegen::middle::*;
use panini_codegen::middle::trace::SourceOrigin;
use panini_codegen::middle::rule::BasicRule;
use panini_codegen::rs;

#[test]
fn test_sequence_ir() {
    let (start, a, b) = (
        rs::str_to_ident("start"),
        rs::str_to_ident("a"),
        rs::str_to_ident("b"),
    );
    let (start, a, b) = (
        rs::dummy_spanned(start.name),
        rs::dummy_spanned(a.name),
        rs::dummy_spanned(b.name),
    );
    let before = Stmts {
        attrs: vec![],
        stmts: vec![
            // start ::= a b;
            Stmt {
                lhs: start,
                rhs: vec![(
                    Rhs(vec![
                        RhsElement {
                            bind: None,
                            elem: RhsAst::Symbol(a),
                        },
                        RhsElement {
                            bind: None,
                            elem: RhsAst::Symbol(b),
                        }
                    ]),
                    ast::Action {
                        expr: None,
                    },
                )],
                ty: None,
                span: rs::DUMMY_SP,
            },
            // a ::= b*;
            Stmt {
                lhs: a,
                rhs: vec![(
                    Rhs(vec![
                        RhsElement {
                            bind: None,
                            elem: RhsAst::Sequence(Sequence {
                                rhs: Rhs(vec![
                                    RhsElement {
                                        bind: None,
                                        elem: RhsAst::Symbol(b),
                                    }
                                ]),
                                min: 0,
                                max: None,
                            }),
                        }
                    ]),
                    ast::Action {
                        expr: None,
                    },
                )],
                ty: None,
                span: rs::DUMMY_SP,
            },
        ],
        lexer: None,
    };
    let result = ir::Ir::transform(before).unwrap();
    // Check grammar
    let mut expected_grammar = Grammar::new();
    let (start_sym, a_sym, b_sym, gen0, gen1) = expected_grammar.sym();
    expected_grammar.rule(start_sym).rhs([a_sym, b_sym]);
    expected_grammar.rule(start_sym).rhs([b_sym]);
    expected_grammar.rule(a_sym).rhs([gen0]);
    expected_grammar.rule(gen0).rhs([gen1]);
    expected_grammar.rule(gen1).rhs([b_sym]);
    expected_grammar.rule(gen1).rhs([gen1, b_sym]);
    support::assert_eq_rules(result.grammar.rules(), expected_grammar.rules().cloned());
    support::assert_eq_origins(result.grammar.rules(), vec![Some(0), Some(0), Some(2), Some(1), None, None]);
    // Check nulling grammar
    let mut expected_nulling_grammar = Grammar::new();
    expected_nulling_grammar.rule(gen0).rhs([]);
    expected_nulling_grammar.rule(a_sym).rhs([gen0]);
    support::assert_eq_rules(result.nulling_grammar.rules(), expected_nulling_grammar.rules());
    support::assert_eq_origins(result.nulling_grammar.rules(), vec![Some(1), Some(2)]);
    // Check type map
    let expected_type_map = hashmap! {
        start_sym => Ty::Auto(AutoTy::Tuple {
            fields: vec![a_sym, b_sym],
        }),
        a_sym => Ty::Auto(AutoTy::Tuple {
            fields: vec![gen0],
        }),
        gen0 => Ty::SequenceVec(b_sym),
    };
    assert_eq!(result.type_map, expected_type_map);
    // Check the absence of trivial derivation
    assert!(!result.trivial_derivation);
    // Check maps
    assert_eq!(result.maps.internalize(start_sym), Some(start_sym));
    assert_eq!(result.maps.internalize(a_sym), Some(a_sym));
    assert_eq!(result.maps.internalize(b_sym), Some(b_sym));
    assert_eq!(result.maps.externalize(start_sym), start_sym);
    assert_eq!(result.maps.name_of_external(start_sym), Some(start.node));
    assert_eq!(result.maps.name_of_external(a_sym), Some(a.node));
    assert_eq!(result.maps.name_of_external(b_sym), Some(b.node));
    // Check type equality checks
    assert!(result.assert_type_equality.is_empty());
    // Check traces
    let expected_trace_tokens = vec![
        vec!["start", "a", "b"],
        vec!["a", "b", "*"],
    ];
    assert_eq!(result.trace_tokens, expected_trace_tokens);
    let expected_trace_sources = vec![
        SourceOrigin { rule_id: 0, rule_pos: vec![0, 1, 2] },
        // sequences do not yet support separators
        SourceOrigin { rule_id: 1, rule_pos: vec![0, 1] },
        SourceOrigin { rule_id: 1, rule_pos: vec![0, 2] },
    ];
    assert_eq!(result.trace_sources, expected_trace_sources);
    // Check actions
    let expected_actions = vec![
        BasicRule {
            lhs: rs::respan(start.span, start_sym),
            rhs: vec![rs::respan(a.span, a_sym), rs::respan(b.span, b_sym)],
            action: Action::Tuple {
                tuple_binds: vec![0, 1],
            },
        },
        BasicRule {
            lhs: rs::dummy_spanned(gen0),
            rhs: vec![rs::respan(b.span, b_sym)],
            action: Action::Sequence,
        },
        BasicRule {
            lhs: rs::respan(a.span, a_sym),
            rhs: vec![rs::dummy_spanned(gen0)],
            action: Action::Tuple {
                tuple_binds: vec![0]
            },
        }
    ];
    assert_eq!(result.basic_rules, expected_actions);
    // Check layering
    assert!(result.lexer_for_upper.is_none());
    assert_eq!(result.lower_level, LowerLevel::None);
}
