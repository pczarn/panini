
fn compiletime_enum_stream_grammar() -> Parser<_, (&rs::Token, rs::Span)> {
    runtime_grammar! {
        // Tokenization is performed by Rust's lexer. Using the enum adaptor to
        // read tokens.
        sub enum_stream {
            not = (&rs::Token::Not, _);
            pound = (&rs::Token::Pound, _);
            comma = (&rs::Token::Comma, _);
            and_and = (&rs::Token::AndAnd, _);
            eq =  (&rs::Token::Eq, _);
            semi = (&rs::Token::Semi, _);
            ident_tok_with_span = (&rs::Token::Ident(_), _);
            l_brace = (&rs::Token::OpenDelim(rs::DelimToken::Brace), _);
            r_brace = (&rs::Token::CloseDelim(rs::DelimToken::Brace), _);
            l_bracket = (&rs::Token::OpenDelim(rs::DelimToken::Bracket), _);
            r_bracket = (&rs::Token::CloseDelim(rs::DelimToken::Bracket), _);
            l_paren = (&rs::Token::OpenDelim(rs::DelimToken::Paren), _);
            r_paren = (&rs::Token::CloseDelim(rs::DelimToken::Paren), _);

            any_token =
                // !(&rs::Token::Ident(ident), _) if ident.as_str() == "if" &&
                !(&rs::Token::OpenDelim(_), _) &&
                !(&rs::Token::CloseDelim(_), _) &&
                !(&rs::Token::Semi, _) &&
                !(&rs::Token::Not, _) &&
                !(&rs::Token::AndAnd, _);
        }

        // null eval!
        start ::= attrs:inner_attr* stmts:stmt* => {
            Stmts::new(attrs, stmts)
        };

        stmt ::= name:name matches rhs:rhs semi => {
            let name: rs::Spanned<rs::Name> = name;
            Stmt {
                name: name.node,
                rhs: rhs,
            }
        };

        rhs ::=
            c:conjunction => {
                StmtRhs {
                    conjunction: c,
                    guard: None,
                }
            }
            | c:conjunction ident:ident guard_cond:tt => {
                let ident: rs::SpannedIdent = ident;
                assert_eq!(&*ident.node.name.as_str(), "if");
                StmtRhs {
                    conjunction: c,
                    guard: Some(guard_cond),
                }
            };

        conjunction ::=
            elems:elem_and* elem:elem => {
                let mut elems: Vec<RhsElem> = elems;
                elems.push(elem);
                elems
            };

        elem_and ::= elem _:and_and;

        elem ::=
            tts:tts => {
                let tts: Vec<rs::TokenTree> = tts;
                RhsElem {
                    pattern: quote_pat!(cx, $tts),
                    positive: true,
                }
            }
            | not tts:tts => {
                let tts: Vec<rs::TokenTree> = tts;
                RhsElem {
                    pattern: quote_pat!(cx, $tts),
                    positive: false,
                }
            };

        inner_attr ::= pound not l_bracket value:meta_item r_bracket => {
            rs::dummy_spanned(rs::Attribute_ {
                id: rs::mk_attr_id(),
                style: rs::ast::AttrStyle::Inner,
                value: value,
                is_sugared_doc: false,
            })
        };

        meta_item ::=
            name:name => {
                let name: rs::Spanned<rs::Name> = name;
                rs::P(rs::dummy_spanned(rs::ast::MetaItemKind::Word(name.node.as_str())))
            }
            | name:name l_paren items:meta_item_list r_paren => {
                let name: rs::Spanned<rs::Name> = name;
                rs::P(rs::dummy_spanned(rs::ast::MetaItemKind::List(name.node.as_str(), items)))
            };

        meta_item_comma ::= meta_item _:comma;

        meta_item_list ::=
            meta_item_comma*
            | v:meta_item_comma* elem:meta_item => {
                let mut v = v;
                v.push(elem);
                v
            };

        matches ::= eq;

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
            | l_brace tts:tts r_brace => {
                delimit(tts, rs::Brace)
            };

        tts ::= tt*;

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
    }
}
