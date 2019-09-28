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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Path {
    pub stmt: usize,
    pub alternative: usize,
    pub position: Vec<usize>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Fragment {
    pub path: Path,
    pub fragment: Option<FragmentId>,
}

pub struct FlattenStmts {
    pub fragments: Vec<Fragment>,
}

impl FlattenStmts {
    pub fn new() -> Self {
        FlattenStmts {
            fragments: vec![],
        }
    }

    pub fn flatten_stmts(&mut self, stmts: Stmts) {
        for (stmt_idx, stmt) in stmts.stmts.iter().enumerate() {
            for (alternative_idx, (_level, ref rhs, ref _action)) in stmt.body.iter().enumerate() {
                let path = Path {
                    stmt: stmt_idx,
                    alternative: alternative_idx,
                    position: vec![],
                };
                self.flatten_rhs(path, rhs);
            }
        }
    }

    fn flatten_rhs(&mut self, path: Path, rhs: &Rhs) {
        for (rhs_idx, element) in rhs.0.iter().enumerate() {
            let path = path.with_position(rhs_idx);
            let mut fragment = Fragment {
                fragment: None,
                path: path.clone(),
            };
            match &element.elem {
                &RhsAst::Fragment(fragment_id) => {
                    fragment.fragment = Some(fragment_id);
                }
                &RhsAst::Sequence(ref sequence) => {
                    self.flatten_rhs(path, &sequence.rhs);
                }
                &RhsAst::Sum(ref summands) => {
                    for (summand_idx, summand) in summands.iter().enumerate() {
                        let path = path.with_position(summand_idx);
                        self.flatten_rhs(path, summand);
                    }
                }
                &RhsAst::Product(ref rhs) => {
                    self.flatten_rhs(path, rhs);
                }
            }
            self.fragments.push(fragment);
        }
    }
}

impl Path {
    fn with_position(&self, new_position: usize) -> Self {
        let mut result = self.clone();
        result.position.push(new_position);
        result
    }
}
