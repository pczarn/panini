use rs;
use lexer::Lexer;
use front::Name;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Stmts {
    pub attrs: Vec<rs::Attribute>,
    pub stmts: Vec<Stmt>,
    pub lexer: Option<Lexer>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Stmt {
    pub lhs: Name,
    pub rhs: PrecedencedLevels,
    pub ty: Option<rs::P<rs::Ty>>,
    pub span: rs::Span,
}

pub type PrecedencedLevels = Vec<AlternativesWithActions>;
pub type AlternativesWithActions = Vec<RhsWithAction>;
pub type RhsWithAction = (Rhs, Action);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Rhs(pub Vec<RhsElement>);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RhsElement {
    pub bind: Option<rs::P<rs::Pat>>,
    pub elem: RhsAst,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RhsAst {
    Symbol(Name),
    Sequence(Sequence),
    Sum(Vec<Rhs>),
    Product(Rhs),
    String(Name),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Sequence {
    pub rhs: Rhs,
    pub min: u32,
    pub max: Option<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Action {
    pub expr: Option<rs::P<rs::Expr>>,
}
