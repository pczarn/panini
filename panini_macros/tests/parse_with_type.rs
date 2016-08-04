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
fn test_simple_parse_with_type() {
    rs::with_fake_extctxt(|ecx| {
        let (start, a, b) = (
            rs::str_to_ident("start"),
            rs::str_to_ident("a"),
            rs::str_to_ident("b"),
        );
        let mut parser = Parser::new();
        let tokens = quote_tokens!(ecx,
            $start -> Test ::= $a $b;
        );
        let result = parser.parse_grammar_from_tts(ecx,
            &tokens[..],
        );
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
                                    elem: RhsAst::Symbol(a),
                                },
                                RhsElement {
                                    bind: None,
                                    elem: RhsAst::Symbol(b),
                                }
                            ]),
                            Action { expr: None },
                        )]
                    ],
                    ty: Some(quote_ty!(ecx, Test)),
                    span: rs::DUMMY_SP,
                },
            ],
            lexer: None
        };
        assert_eq!(result, after);
    });
}
