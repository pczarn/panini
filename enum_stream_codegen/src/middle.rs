use rs;
use front;

pub type Name = rs::Name;

pub struct Ir {
    pub rules: Vec<Rule>,
    pub level: usize,
}

pub struct Rule {
    pub name: Name,
    pub negative: Vec<(Pat, Guard)>,
    pub positive: Vec<(Pat, Guard)>,
}

pub type Pat = rs::P<rs::Pat>;
pub type Guard = Option<rs::P<rs::Expr>>;

impl Ir {
    fn new() -> Self {
        Ir { rules: vec![], level: 0 }
    }
}

pub fn transform(stmts: front::Stmts) -> Result<Ir, ()> {
    let mut ir = Ir::new();
    for stmt in stmts.stmts {
        let mut negative = vec![];
        let mut positive = vec![];
        for (elem, guard) in stmt.rhs.conjunction {
            let v = if elem.positive {
                &mut positive
            } else {
                &mut negative
            };
            v.push((elem.pattern, guard));
        }
        ir.rules.push(Rule {
            name: stmt.name,
            positive: positive,
            negative: negative,
        });
    }
    ir.level = stmts.level;
    Ok(ir)
}
