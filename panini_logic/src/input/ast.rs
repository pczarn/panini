use input::{AttrId, TyId, LexerId, BindId, FragmentId, ExprId};

#[derive(Clone, Debug)]
pub struct Stmts {
    pub attrs: Vec<AttrId>,
    pub stmts: Vec<Stmt>,
    pub lexer: Option<LexerId>,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub lhs: FragmentId,
    pub body: Vec<Alternative>,
    pub ty: Option<TyId>,
}

pub type Alternative = (Level, Rhs, Action);

type Level = u32;

#[derive(Clone, Debug)]
pub struct Rhs(pub Vec<RhsElement>);

#[derive(Clone, Debug)]
pub struct Action {
    pub expr: Expr,
}

#[derive(Clone, Debug)]
pub struct RhsElement {
    pub bind: Bind,
    pub elem: RhsAst,
}

pub type Bind = Option<BindId>;

#[derive(Clone, Debug)]
pub enum RhsAst {
    Fragment(FragmentId),
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

pub type Expr = Option<ExprId>;
