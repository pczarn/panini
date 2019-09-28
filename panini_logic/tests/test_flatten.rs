extern crate cfg;
extern crate panini_logic;

use cfg::Cfg;
use cfg::ContextFree;

use panini_logic::input::{Stmts, Stmt, RhsAst, Rhs, RhsElement, Action, Sequence};
use panini_logic::input::ast::{FlattenStmts, Fragment, Path};

#[test]
fn test_fragments() {
    let (start, a, b) = (0, 1, 2);
    let stmts = Stmts {
        attrs: vec![],
        stmts: vec![
            // start ::= a b;
            Stmt {
                lhs: start,
                body: vec![
                    (
                        0,
                        Rhs(vec![
                            RhsElement {
                                bind: None,
                                elem: RhsAst::Fragment(a),
                            },
                            RhsElement {
                                bind: None,
                                elem: RhsAst::Fragment(b),
                            }
                        ]),
                        Action {
                            expr: None,
                        }
                    )
                ],
                ty: None,
            },
            // a ::= b*;
            Stmt {
                lhs: a,
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
                                            elem: RhsAst::Fragment(b),
                                        }
                                    ]),
                                    min: 0,
                                    max: None,
                                }),
                            }
                        ]),
                        Action {
                            expr: None,
                        },
                    )
                ],
                ty: None,
            },
        ],
        lexer: None,
    };
    let mut flatten = FlattenStmts::new();
    flatten.flatten_stmts(stmts);
    let expected_fragments = vec![
        Fragment {
            path: Path {
                stmt: 0,
                alternative: 0,
                position: vec![0]
            },
            fragment: Some(1)
        },
        Fragment {
            path: Path {
                stmt: 0,
                alternative: 0,
                position: vec![1]
            },
            fragment: Some(2)
        },
        Fragment {
            path: Path {
                stmt: 1,
                alternative: 0,
                position: vec![0, 0]
            },
            fragment: Some(2)
        },
        Fragment {
            path: Path {
                stmt: 1,
                alternative: 0,
                position: vec![0]
            },
            fragment: None
        }
    ];
    assert_eq!(flatten.fragments, expected_fragments);
    let mut expected_cfg = Cfg::new();
    let (start_sym, a_sym, b_sym) = expected_cfg.sym();
    expected_cfg.rule(start_sym).rhs(&[a_sym, b_sym]);
    expected_cfg.sequence(a_sym).inclusive(0, None).rhs(b_sym);
}
