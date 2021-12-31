use std::mem;

use panini_codegen::rs;
use panini_codegen::front::lexer::Lexer;
use panini_codegen::front::{Stmts, Stmt, Product, Factor, RhsElement, Action, Sequence};

use util::delimit;

pub fn compiletime_grammar_grammar() -> Parser<_, (&rs::Token, rs::Span)> {
    runtime_grammar! {
        // Tokenization is performed by Rust's lexer. Using the enum adaptor to
        // read tokens.
        sub enum_stream -> ((&rs::Token, rs::Span)) {
            not = (&rs::Token::Not, _);
            pound = (&rs::Token::Pound, _);
            comma = (&rs::Token::Comma, _);
            underscore = (&rs::Token::Underscore, _);
            colon = (&rs::Token::Colon, _);
            mod_sep = (&rs::Token::ModSep, _);
            eq =  (&rs::Token::Eq, _);
            rarrow = (&rs::Token::RArrow, _);
            fat_arrow = (&rs::Token::FatArrow, _);
            semi = (&rs::Token::Semi, _);
            star = (&rs::Token::BinOp(rs::BinOpToken::Star), _);
            plus = (&rs::Token::BinOp(rs::BinOpToken::Plus), _);
            pipe = (&rs::Token::BinOp(rs::BinOpToken::Or), _);
            ident_tok_with_span = (&rs::Token::Ident(_), _);
            l_brace = (&rs::Token::OpenDelim(rs::DelimToken::Brace), _);
            r_brace = (&rs::Token::CloseDelim(rs::DelimToken::Brace), _);
            l_bracket = (&rs::Token::OpenDelim(rs::DelimToken::Bracket), _);
            r_bracket = (&rs::Token::CloseDelim(rs::DelimToken::Bracket), _);
            l_paren = (&rs::Token::OpenDelim(rs::DelimToken::Paren), _);
            r_paren = (&rs::Token::CloseDelim(rs::DelimToken::Paren), _);
            r_angle = (&rs::Token::Gt, _);
            string = (&rs::Token::Literal(rs::token::Str_(_), _), _);
            sub = ((&rs::Token::Ident(ident), _) if (ident.name.as_str() == "sub"));

            any_token = (
                (!(&rs::Token::OpenDelim(_), _)) &&
                (!(&rs::Token::CloseDelim(_), _))
            );
        }

        start -> Stmts ::= (((attrs:inner_attr)*) ((stmts:stmt)*) (lexer:lexer) ((stages:stage)*)) => {
            Stmts {
                attrs: attrs,
                stmts: stmts,
                lexer: lexer,
                stages: stages,
            }
        };

        inner_attr ::=
            ((leftmost:pound) not l_bracket (value:meta_item) (rightmost:r_bracket)) => {
                // Inference is failing us here.
                let (_, leftmost_span): (&rs::Token, rs::Span) = leftmost;
                let (_, rightmost_span): (&rs::Token, rs::Span) = rightmost;
                let attr = rs::Attribute_ {
                    id: rs::mk_attr_id(),
                    style: rs::ast::AttrStyle::Inner,
                    value: value,
                    is_sugared_doc: false,
                };
                let mut ret = rs::spanned(leftmost_span.lo, rightmost_span.hi, attr);
                ret.span.expn_id = leftmost_span.expn_id;
                ret
            };

        meta_item ::=
            (name:name) => {
                let name: rs::Spanned<rs::Name> = name;
                let word = rs::ast::MetaItemKind::Word(name.node.as_str());
                rs::P(rs::respan(name.span, word))
            }
            | ((name:name) l_paren (items:meta_item_list) (rightmost:r_paren)) => {
                let name: rs::Spanned<rs::Name> = name;
                let (_, rightmost_span): (&rs::Token, rs::Span) = rightmost;
                let list = rs::ast::MetaItemKind::List(name.node.as_str(), items);
                let mut ret = rs::spanned(name.span.lo, rightmost_span.hi, list);
                ret.span.expn_id = name.span.expn_id;
                rs::P(ret)
            };

        meta_item_comma ::= ((meta_item:meta_item) comma) => { meta_item };

        meta_item_list ::=
            ((elem:meta_item_comma)*) => { elem }
            | (((v:meta_item_comma)*) (elem:meta_item)) => {
                let mut v = v;
                v.push(elem);
                v
            };

        stmt ::= ((lhs:name) (ty:ty) defined_as (rhs:precedenced) semi) => {
            Stmt {
                lhs: lhs,
                rhs: rhs,
                ty: ty,
                span: rs::DUMMY_SP,
            }
        };

        defined_as ::= mod_sep eq => {};

        ty ::=
            (rarrow (tt:tt)) => {
                Some(tt)
            }
            | () => {
                None
            };

        action ::=
            (fat_arrow l_brace (tts:tts) r_brace) => {
                Some(tts)
            }
            | () => {
                None
            };

        tt ::=
            (t:any_token) => {
                let (t, _): (&rs::Token, _) = t;
                rs::TokenTree::Token(rs::DUMMY_SP, (*t).clone())
            }
            | (l_bracket (tts:tts) r_bracket) => {
                delimit(tts, rs::Bracket)
            }
            | (l_paren (tts:tts) r_paren) => {
                delimit(tts, rs::Paren)
            }
            | (elem:brace_tt) => {
                elem
            };

        brace_tt ::= (l_brace (tts:tts) r_brace) => {
            delimit(tts, rs::Brace)
        };

        tts ::= ((tts:tt)*);

        pattern ::=
            ((ident:ident) colon) => {
                let ident: rs::SpannedIdent = ident;
                Some(rs::respan(ident.span, ident.node))
            }
            | (underscore colon) => {
                Some(quote! { _ })
            }
            | () => {
                None
            };

        precedenced ::=
            (alt:top_rhs) => {
                vec![alt]
            }
            | ((rec:precedenced) pipe r_angle (alt:top_rhs)) => {
                let mut rec = rec;
                rec.push(alt);
                rec
            };

        top_rhs ::=
            (((elems:pat_elem)*) block:action) => {
                let product = create_entity().with(Product(elems)).with(Action { expr: block });
                vec![product]
            }
            | ((v:top_rhs) pipe ((elems:pat_elem)*) (block:action)) => {
                let mut v = v;
                let product = create_entity().with(Product(elems)).with(Action { expr: block });
                v.push(product);
                v
            };

        alt ::=
            ((elems:pat_elem)*) => {
                vec![Rhs(elems)]
            }
            | ((v:alt) pipe ((elems:pat_elem)*)) => {
                let mut v = v;
                v.push(Rhs(elems));
                v
            };

        pat_elem ::= ((pat:pattern) (elem:elem)) => {
            RhsElement {
                bind: pat,
                elem: elem,
            }
        };

        elem ::=
            (sym:name) => {
                RhsAst::Symbol(sym)
            }
            | (l_paren (alt:alt) r_paren) => {
                RhsAst::Sum(alt)
            }
            | ((rhs:rhs_elem) star) => {
                RhsAst::Sequence(Sequence{
                    rhs: rhs,
                    min: 0,
                    max: None,
                })
            }
            | ((rhs:rhs_elem) plus) => {
                RhsAst::Sequence(Sequence{
                    rhs: rhs,
                    min: 1,
                    max: None,
                })
            }
            | (s:string) => {
                let (t, sp) = if let (&rs::token::Literal(rs::token::Str_(t), _), sp) = s {
                    (t, sp)
                } else {
                    panic!();
                };
                RhsAst::String(rs::respan(sp, t))
            };

        rhs_elem ::= (elem:elem) => {
            let mut v = Vec::new();
            v.push(RhsElement {
                bind: None,
                elem: elem,
            });
            Rhs(v)
        };

        lexer ::=
            sub name:name l_brace tts:tts r_brace => {
                Some(Lexer::new(name.node, tts))
            }
            | => {
                None
            };

        stage ::=
            pound l_bracket id:stage_id r_bracket l_brace (tts:tts) r_brace => {
                Stage::new(id, tts)
            };

        name ::= (i:ident) => {
            let i: rs::SpannedIdent = i;
            rs::respan(i.span, i.node.name)
        };

        ident ::= (i:ident_tok_with_span) => {
            match i {
                (&rs::Token::Ident(ident), sp) => rs::respan(sp, ident),
                _ => loop {}
            }
        };
    }
}
