use std::mem;

use aster::AstBuilder;

use panini_codegen::rs;
use panini_codegen::front::lexer::Lexer;
use panini_codegen::front::{Stmts, Stmt, Rhs, RhsAst, RhsElement, Action, Sequence};

use util::delimit;

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    pub fn parse_grammar_from_tts(&mut self, cx: &rs::ExtCtxt, tts: &[rs::TokenTree]) -> Stmts {
        let sess = cx.parse_sess();
        let mut trdr = rs::lexer::new_tt_reader(&sess.span_diagnostic, None, None, tts.to_vec());

        let mut spans = vec![];
        let mut tokens = vec![];
        let mut token_and_span = rs::transcribe::tt_next_token(&mut trdr);
        while token_and_span.tok != rs::Token::Eof {
            let t = mem::replace(&mut token_and_span, rs::transcribe::tt_next_token(&mut trdr));
            spans.push(t.sp);
            tokens.push(t.tok);
        }
        self.parse_grammar(cx, &tokens[..], &spans[..])
    }

    pub fn parse_grammar(
        &mut self,
        cx: &rs::ExtCtxt,
        tokens: &[rs::Token],
        spans: &[rs::Span])
        -> Stmts
    {
        let mut parser = grammar! {
            start ::= attrs:inner_attr* stmts:stmt* lexer:lexer => {
                Stmts {
                    attrs: attrs,
                    stmts: stmts,
                    lexer: lexer,
                }
            };

            inner_attr ::=
                leftmost:pound not l_bracket value:meta_item rightmost:r_bracket => {
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
                name:name => {
                    let name: rs::Spanned<rs::Name> = name;
                    let word = rs::ast::MetaItemKind::Word(name.node.as_str());
                    rs::P(rs::respan(name.span, word))
                }
                | name:name l_paren items:meta_item_list rightmost:r_paren => {
                    let name: rs::Spanned<rs::Name> = name;
                    let (_, rightmost_span): (&rs::Token, rs::Span) = rightmost;
                    let list = rs::ast::MetaItemKind::List(name.node.as_str(), items);
                    let mut ret = rs::spanned(name.span.lo, rightmost_span.hi, list);
                    ret.span.expn_id = name.span.expn_id;
                    rs::P(ret)
                };

            meta_item_comma ::= meta_item _:comma;

            meta_item_list ::=
                meta_item_comma*
                | v:meta_item_comma* elem:meta_item => {
                    let mut v = v;
                    v.push(elem);
                    v
                };

            stmt ::= lhs:name ty:ty defined_as rhs:precedenced semi => {
                Stmt {
                    lhs: lhs,
                    rhs: rhs,
                    ty: ty,
                    span: rs::DUMMY_SP,
                }
            };

            defined_as ::= mod_sep eq;

            ty ::=
                rarrow tt:tt => {
                    Some(quote_ty!(cx, $tt))
                }
                | => {
                    None
                };

            action ::=
                fat_arrow l_brace tts:tts r_brace => {
                    Some(quote_expr!(cx, { $tts }))
                }
                | => {
                    None
                };

            tt ::=
                t:any_token => {
                    let (t, _): (&rs::Token, _) = t;
                    rs::TokenTree::Token(rs::DUMMY_SP, (*t).clone())
                }
                | l_bracket tts:tts r_bracket => {
                    delimit(tts, rs::Bracket)
                }
                | l_paren tts:tts r_paren => {
                    delimit(tts, rs::Paren)
                }
                | brace_tt;

            brace_tt ::= l_brace tts:tts r_brace => {
                delimit(tts, rs::Brace)
            };

            tts ::= tt*;

            pattern ::=
                ident:ident colon => {
                    let ident: rs::SpannedIdent = ident;
                    Some(AstBuilder::new().span(ident.span).pat().id(ident.node))
                }
                | underscore colon => {
                    Some(AstBuilder::new().pat().wild())
                }
                | => {
                    None
                };

            precedenced ::=
                alt:top_rhs => {
                    vec![alt]
                }
                | rec:precedenced pipe r_angle alt:top_rhs => {
                    let mut rec = rec;
                    rec.push(alt);
                    rec
                };

            top_rhs ::=
                elems:pat_elem* block:action => {
                    vec![(Rhs(elems), Action { expr: block })]
                }
                | v:top_rhs pipe elems:pat_elem* block:action => {
                    let mut v = v;
                    v.push((Rhs(elems), Action { expr: block }));
                    v
                };

            alt ::=
                elems:pat_elem* => {
                    vec![Rhs(elems)]
                }
                | v:alt pipe elems:pat_elem* => {
                    let mut v = v;
                    v.push(Rhs(elems));
                    v
                };

            pat_elem ::= pat:pattern elem:elem => {
                RhsElement {
                    bind: pat,
                    elem: elem,
                }
            };

            elem ::=
                sym:name => {
                    RhsAst::Symbol(sym)
                }
                | l_paren alt:alt r_paren => {
                    RhsAst::Sum(alt)
                }
                | rhs:rhs_elem star => {
                    RhsAst::Sequence(Sequence{
                        rhs: rhs,
                        min: 0,
                        max: None,
                    })
                }
                | rhs:rhs_elem plus => {
                    RhsAst::Sequence(Sequence{
                        rhs: rhs,
                        min: 1,
                        max: None,
                    })
                }
                | s:string => {
                    let (t, sp) = if let (&rs::token::Literal(rs::token::Str_(t), _), sp) = s {
                        (t, sp)
                    } else {
                        panic!();
                    };
                    RhsAst::String(rs::respan(sp, t))
                };

            rhs_elem ::= elem:elem => {
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

            name ::= i:ident => {
                let i: rs::SpannedIdent = i;
                rs::respan(i.span, i.node.name)
            };

            ident ::= i:ident_tok_with_span => {
                match i {
                    (&rs::Token::Ident(ident), sp) => rs::respan(sp, ident),
                    _ => loop {}
                }
            };

            // Tokenization is performed by Rust's lexer. Using the enum adaptor to
            // read tokens.
            sub enum_stream {
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
                sub = (&rs::Token::Ident(ident), _) if (ident.name.as_str() == "sub");

                any_token =
                    !(&rs::Token::OpenDelim(_), _) &&
                    !(&rs::Token::CloseDelim(_), _);
            }
        };

        let mut result = parser.parse(tokens.iter().zip(spans.iter().cloned())).unwrap();
        let stmts = result.next().unwrap().clone();
        assert!(result.next().is_none());
        stmts
    }
}
