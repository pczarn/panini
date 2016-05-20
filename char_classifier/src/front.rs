use std::mem;
use std::rc::Rc;

use aster::AstBuilder;

use rs;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub attrs: Vec<rs::Attribute>,
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub lhs: rs::Spanned<rs::Name>,
    pub range: ClassRange,
    pub span: rs::Span,
}

#[derive(Clone, Debug)]
pub struct ClassRange {
    pub start: char,
    pub end: char,
}

pub struct Parser;

impl Parser {
    pub fn new() -> Self {
        Parser
    }

    pub fn parse_from_tts(&mut self, cx: &mut rs::ExtCtxt, tts: &[rs::TokenTree]) -> Stmts {
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

    pub fn parse_grammar(&mut self, cx: &mut rs::ExtCtxt, tokens: &[rs::Token], spans: &[rs::Span])
        -> Stmts
    {
        let mut parser = grammar! {
            start ::= attrs:inner_attr* stmts:stmt* => {
                Stmts {
                    attrs: attrs,
                    stmts: stmts,
                }
            };

            // FIXME needed type declaration?
            inner_attr -> (rs::Attribute) ::= pound not l_bracket value:meta_item r_bracket => {
                rs::dummy_spanned(rs::Attribute_ {
                    id: rs::mk_attr_id(),
                    style: rs::ast::AttrStyle::Inner,
                    value: value,
                    is_sugared_doc: false,
                })
            };

            meta_item -> (rs::P<rs::ast::MetaItem>) ::=
                name:name => {
                    let name: rs::Spanned<rs::Name> = name;
                    rs::P(rs::dummy_spanned(rs::ast::MetaItemKind::Word(name.node.as_str())))
                }
                | name:name l_paren items:meta_item_list r_paren => {
                    let name: rs::Spanned<rs::Name> = name;
                    rs::P(rs::dummy_spanned(rs::ast::MetaItemKind::List(name.node.as_str(), items)))
                };
                // | ident:ident eq lit => { MetaItemKind::NameValue(ident, lit) };

            meta_item_comma ::= meta_item _:comma;

            meta_item_list ::=
                meta_item_comma*
                | v:meta_item_comma* elem:meta_item => {
                    let mut v = v;
                    v.push(elem);
                    v
                };

            stmt ::= lhs:name defined_as rhs:rhs semi => {
                Stmt {
                    lhs: lhs,
                    range: rhs,
                    span: rs::DUMMY_SP,
                }
            };

            defined_as ::= mod_sep eq;

            rhs ::= ch1:character dotdotdot ch2:character => {
                let ch1 = if let (&rs::Token::Literal(rs::token::Char(ch), _), _) = ch1 {
                    ch
                } else {
                    unreachable!()
                };
                let ch2 = if let (&rs::Token::Literal(rs::token::Char(ch), _), _) = ch2 {
                    ch
                } else {
                    unreachable!()
                };
                let ch1 = ch1.as_str().chars().next().unwrap();
                let ch2 = ch2.as_str().chars().next().unwrap();
                ClassRange {
                    start: ch1,
                    end: ch2,
                }
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
                mod_sep = (&rs::Token::ModSep, _);
                eq = (&rs::Token::Eq, _);
                dotdotdot = (&rs::Token::DotDotDot, _);
                semi = (&rs::Token::Semi, _);
                ident_tok_with_span = (&rs::Token::Ident(_), _);
                l_bracket = (&rs::Token::OpenDelim(rs::DelimToken::Bracket), _);
                r_bracket = (&rs::Token::CloseDelim(rs::DelimToken::Bracket), _);
                l_paren = (&rs::Token::OpenDelim(rs::DelimToken::Paren), _);
                r_paren = (&rs::Token::CloseDelim(rs::DelimToken::Paren), _);
                character = (&rs::Token::Literal(rs::token::Char(_), _), _);
            }
        };

        let mut result = parser.parse(tokens.iter().zip(spans.iter().cloned())).unwrap();
        let stmts = result.next().unwrap().clone();
        assert!(result.next().is_none());
        stmts
    }
}
