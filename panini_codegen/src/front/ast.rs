use rs;
use lexer::Lexer;
use front::Name;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub attrs: Vec<rs::TokenStream>,
    pub stmts: Vec<Stmt>,
    pub lexer: Option<Lexer>,
}

pub type Ty = Option<rs::TokenStream>;

#[derive(Clone, Debug)]
pub struct Stmt {
    pub lhs: Name,
    pub rhs: PrecedencedLevels,
    pub ty: Ty,
    pub span: rs::Span,
}

pub type PrecedencedLevels = Vec<AlternativesWithActions>;
pub type AlternativesWithActions = Vec<RhsWithAction>;
pub type RhsWithAction = (Rhs, Action);

#[derive(Clone, Debug)]
pub struct Rhs(pub Vec<RhsElement>);

pub type Bind = Option<rs::TokenStream>;

#[derive(Clone, Debug)]
pub struct RhsElement {
    pub bind: Bind,
    pub elem: RhsAst,
}

#[derive(Clone, Debug)]
pub enum RhsAst {
    Symbol(Name),
    Sequence(Sequence),
    Sum(Vec<Rhs>),
    Product(Rhs),
    String(Name),
}

#[derive(Clone, Debug)]
pub struct Sequence {
    pub rhs: Rhs,
    pub min: u32,
    pub max: Option<u32>,
}

pub type Expr = Option<rs::TokenStream>;

#[derive(Clone, Debug)]
pub struct Action {
    pub expr: Expr,
}
