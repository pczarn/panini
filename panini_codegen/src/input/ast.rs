use input::{AttrId, TyId, LexerId, BindId, FragmentId, ExprId, SpanId};
use input::attr_arguments::AttrArguments;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub attr_arguments: AttrArguments,
    pub stmts: Vec<Stmt>,
    pub lexer: Option<LexerId>,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub lhs: rs::Term,
    pub body: Vec<Alternative>,
    pub ty: Option<rs::TokenStream>,
}

pub type Alternative = (Level, Rhs, Action);

type Level = u32;

#[derive(Clone, Debug)]
pub struct Rhs(pub Vec<RhsElement>);

#[derive(Clone, Debug)]
pub struct Action {
    pub expr: Option<ExprId>,
}

#[derive(Clone, Debug)]
pub struct RhsElement {
    pub bind: Bind,
    pub elem: RhsAst,
}

pub type Bind = Option<BindId>;

#[derive(Clone, Debug)]
pub enum RhsAst {
    Symbol(rs::Term),
    String(String),
    Sequence(Sequence),
    Sum(Vec<Rhs>),
    Product(Rhs),
}

#[derive(Clone, Debug)]
pub struct Sequence {
    pub rhs: Rhs,
    pub min: u32,
    pub max: Option<u32>,
}
