use rs;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub stmts: Vec<Stmt>,
    pub level: usize,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub ident: rs::Ident,
    pub rhs: StmtRhs,
}

#[derive(Clone, Debug)]
pub struct StmtRhs {
    pub conjunction: Vec<RhsElem>,
    pub guard: Option<rs::Expr>,
}

#[derive(Clone, Debug)]
pub struct RhsElem {
    pub pattern: rs::Pat,
    pub positive: bool,
}

impl Stmts {
    pub fn new(attrs: Vec<rs::Attribute>, stmts: Vec<Stmt>) -> Self {
        let mut level = 0;
        for attr in &attrs {
            match attr.parse_meta() {
                Ok(rs::Meta::List(list)) => {
                    let name = list.ident.to_string();
                    if name.starts_with("lexer_") {
                        // TODO: check for available terminals.
                        level = name["lexer_".len() ..].parse().unwrap();
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
