use rs;
use lexer::Lexer;
use front::Name;
use specs::Entity;

// entities

#[derive(Clone, Debug)]
pub struct Stmts {
    pub attrs: Vec<rs::TokenStream>,
    pub stmts: Vec<Entity>,
    pub lexer: Option<Lexer>,
}

pub type Ty = Option<rs::TokenStream>;

#[derive(Clone, Debug)]
pub struct Stmt {
    pub lhs: Symbol,
    pub rhs: Atom,
    pub ty: Ty,
}

struct Spanned {
    pub span: rs::Span,
}

// non-entities

pub type PrecedencedLevels = Vec<SumEntity>;

struct Sum {
    pub summands: Vec<Entity>
}

#[derive(Clone, Debug)]
pub struct Product {
    pub factors: Vec<Entity>
}

pub struct Bind {
    tokens: Option<rs::TokenStream>
}

#[derive(Clone, Debug)]
pub struct Factor {
    pub bind: Entity,
    pub elem: Entity,
}

#[derive(Clone, Debug)]
pub enum Atom {
    Symbol(Entity),
    Literal(Entity),
    Product(Entity),
    Sum(Entity),
    Sequence(Entity),
    Precedenced(Entity),
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

struct Symbol {
    entity: Entity,
}
type Symbol = EntityWith<Symbol>;









#[derive(Clone)]
pub enum Rule<S = Symbol> {
    BnfRule {
        lhs: SymbolEntity,
        rhs: Vec<SymbolEntity>,
        tuple_binds: Vec<usize>,
        deep_binds: Vec<usize>,
        shallow_binds: Vec<(usize, rs::Ident)>,
        action: ActionExpr,
        source_origin: SourceOrigin,
    },
    SequenceRule {
        lhs: SymbolEntity,
        rhs: SymbolEntity,
        min: u32,
        max: Option<u32>,
        action: ActionExpr,
        source_origin: SourceOrigin,
    },
    PrecedencedRule {
        lhs: SymbolEntity,
        rhs_levels: Vec<PrecedenceLevel<S>>,
    },
}

#[derive(Clone, Debug)]
pub struct PrecedenceLevel<S> {
    pub rules: Vec<PrecedencedRuleAlternative<S>>,
}

#[derive(Clone, Debug)]
pub struct PrecedencedRuleAlternative<S> {
    pub rhs: Vec<SymbolEntity>,
    pub tuple_binds: Vec<usize>,
    pub deep_binds: Vec<usize>,
    pub shallow_binds: Vec<(usize, rs::Ident)>,
    pub action: ActionExpr,
    pub source_origin: SourceOrigin,
}
