pub struct SpannedToken {
    token: Token,
    span: Span,
}

pub enum Token {
    Not,
    Pound,
    Comma,
    Underscore,
    Colon,
    ModSep,
    Eq,
    RArrow,
    FatArrow,
    Semi,
    BinOp(BinOpToken),
    Ident(Ident),
    OpenDelim(DelimToken),
    CloseDelim(DelimToken),
    Gt,
    Literal(String),
}

pub enum BinOpToken {
    Star,
    Plus,
    Or,
}

pub enum DelimToken {
    Brace,
    Bracket,
    Paren,
}

pub struct Ident {
    name: String,
}

// not = (&rs::Token::Not, _);
// pound = (&rs::Token::Pound, _);
// comma = (&rs::Token::Comma, _);
// underscore = (&rs::Token::Underscore, _);
// colon = (&rs::Token::Colon, _);
// mod_sep = (&rs::Token::ModSep, _);
// eq =  (&rs::Token::Eq, _);
// rarrow = (&rs::Token::RArrow, _);
// fat_arrow = (&rs::Token::FatArrow, _);
// semi = (&rs::Token::Semi, _);
// star = (&rs::Token::BinOp(rs::BinOpToken::Star), _);
// plus = (&rs::Token::BinOp(rs::BinOpToken::Plus), _);
// pipe = (&rs::Token::BinOp(rs::BinOpToken::Or), _);
// ident_tok_with_span = (&rs::Token::Ident(_), _);
// l_brace = (&rs::Token::OpenDelim(rs::DelimToken::Brace), _);
// r_brace = (&rs::Token::CloseDelim(rs::DelimToken::Brace), _);
// l_bracket = (&rs::Token::OpenDelim(rs::DelimToken::Bracket), _);
// r_bracket = (&rs::Token::CloseDelim(rs::DelimToken::Bracket), _);
// l_paren = (&rs::Token::OpenDelim(rs::DelimToken::Paren), _);
// r_paren = (&rs::Token::CloseDelim(rs::DelimToken::Paren), _);
// r_angle = (&rs::Token::Gt, _);
// string = (&rs::Token::Literal(rs::token::Str_(_), _), _);
// sub = ((&rs::Token::Ident(ident), _) if (ident.name.as_str() == "sub"));