#![feature(plugin, rustc_private)]
#![plugin(quasi_macros)]

#![allow(plugin_as_library)]

extern crate syntax;
extern crate quasi;
extern crate panini_codegen;
extern crate panini_macros;

use panini_codegen::*;
use panini_codegen::front::ast::*;
use panini_macros::*;

#[test]
fn test_sequence() {
    rs::with_fake_extctxt(|ecx| {
        let (start, a, b) = (
            rs::str_to_ident("start"),
            rs::str_to_ident("a"),
            rs::str_to_ident("b"),
        );
        let mut parser = Parser::new();
        let tokens = quote_tokens!(ecx,
            $start ::= $a*;
            $b ::= $a $b+ $a;
        );
        let result = parser.parse_grammar_from_tts(ecx, &tokens[..]);
        let (start, a, b) = (
            rs::dummy_spanned(start.name),
            rs::dummy_spanned(a.name),
            rs::dummy_spanned(b.name),
        );
        let after = Stmts {
            attrs: vec![],
            stmts: vec![
                Stmt {
                    lhs: start,
                    rhs: vec![
                        vec![(
                            Rhs(vec![
                                RhsElement {
                                    bind: None,
                                    elem: RhsAst::Sequence(Sequence {
                                        rhs: Rhs(vec![
                                            RhsElement {
                                                bind: None,
                                                elem: RhsAst::Symbol(a),
                                            }
                                        ]),
                                        min: 0,
                                        max: None,
                                    }),
                                },
                            ]),
                            Action {
                                expr: None,
                            },
                        )]
                    ],
                    ty: None,
                    span: rs::DUMMY_SP,
                },
                Stmt {
                    lhs: b,
                    rhs: vec![
                        vec![(
                            Rhs(vec![
                                RhsElement {
                                    bind: None,
                                    elem: RhsAst::Symbol(a),
                                },
                                RhsElement {
                                    bind: None,
                                    elem: RhsAst::Sequence(Sequence {
                                        rhs: Rhs(vec![
                                            RhsElement {
                                                bind: None,
                                                elem: RhsAst::Symbol(b),
                                            }
                                        ]),
                                        min: 1,
                                        max: None,
                                    }),
                                },
                                RhsElement {
                                    bind: None,
                                    elem: RhsAst::Symbol(a),
                                },
                            ]),
                            Action {
                                expr: None,
                            },
                        )]
                    ],
                    ty: None,
                    span: rs::DUMMY_SP,
                },
            ],
            lexer: None
        };
        assert_eq!(result, after);
    });
}
