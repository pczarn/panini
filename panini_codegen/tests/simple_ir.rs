#![feature(rustc_private)]

extern crate gearley;
extern crate panini_codegen;

use std::collections::HashMap;

use gearley::grammar::Grammar;

use panini_codegen::front::ast::{self, Stmts, Stmt, RhsElement, Rhs, RhsAst};
use panini_codegen::middle::*;
use panini_codegen::rs;

#[test]
fn test_simple_parse() {

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
        ],
        lexer: None,
    };
    let result = ir::Ir::transform(before).unwrap();
    // Expected results
    let mut expected_grammar = Grammar::new();
    let (start_sym, a_sym, b_sym) = expected_grammar.sym();
    expected_grammar.rule(start_sym).rhs([a_sym, b_sym]);
    let mut expected_type_map = HashMap::new();
    expected_type_map.insert(start_sym, Ty::Auto(AutoTy::Tuple {
        fields: vec![a_sym, b_sym],
    }));
    assert_eq!(result.type_map, expected_type_map);
    assert!(!result.trivial_derivation);
}
