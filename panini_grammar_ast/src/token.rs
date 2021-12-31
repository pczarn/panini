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