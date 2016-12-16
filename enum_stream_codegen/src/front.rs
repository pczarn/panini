use rs;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub stmts: Vec<Stmt>,
    pub level: usize,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub name: rs::Name,
    pub rhs: StmtRhs,
}

#[derive(Clone, Debug)]
pub struct StmtRhs {
    pub conjunction: Vec<(RhsElem, Guard)>,
}

#[derive(Clone, Debug)]
pub struct RhsElem {
    pub pattern: rs::P<rs::Pat>,
    pub positive: bool,
}

pub type Guard = Option<rs::P<rs::Expr>>;

impl Stmts {
    pub fn new(attrs: Vec<rs::Attribute>, stmts: Vec<Stmt>) -> Self {
        let mut level = 0;
        for attr in &attrs {
            match &attr.node.value.node {
                &rs::ast::MetaItemKind::List(ref s, ref list) => {
                    if s.starts_with("lexer_") {
                        // TODO: check for available terminals.
                        level = s["lexer_".len() ..].parse().unwrap();
                    }
                }
                _ => {}
            }
        }
        Stmts {
            stmts: stmts,
            level: level,
        }
    }
}
