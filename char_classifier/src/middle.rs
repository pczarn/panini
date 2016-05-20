use rs;
use front;

pub type Name = rs::Name;

pub struct Ir {
    pub rules: Vec<Rule>,
    pub level: usize,
}

pub struct Rule {
    pub lhs: Name,
    pub start: char,
    pub end: char,
}

impl Ir {
    fn new() -> Self {
        Ir {
            rules: vec![],
            level: 0
        }
    }
}

pub fn transform(stmts: front::Stmts) -> Result<Ir, ()> {
    let mut ir = Ir::new();
    for stmt in stmts.stmts {
        ir.rules.push(Rule {
            lhs: stmt.lhs.node,
            start: stmt.range.start,
            end: stmt.range.end,
        });
    }
    for attr in &stmts.attrs {
        match &attr.node.value.node {
            &rs::ast::MetaItemKind::List(ref s, ref list) => {
                if s.starts_with("lexer_") {
                    // for word in list {
                    //     match &word.node {
                    //         &rs::ast::MetaItemKind::Word(ref terminal) => {
                    //             let name = rs::intern(&*terminal);
                    //             upper_level_terminals.push(rs::dummy_spanned(name));
                    //         }
                    //         _ => unreachable!() // error
                    //     }
                    // }
                    // start = rs::gensym("_lower_start_");
                    // assert!(!upper_level_terminals.is_empty());
                    // for &upper_terminal in &upper_level_terminals {
                    //     // let rhs_sym = fold.fold_symbol(upper_terminal);
                    //     stmts.stmts.push(ast::Stmt {
                    //         lhs: rs::dummy_spanned(start),
                    //         rhs: vec![(
                    //             ast::Rhs(vec![ast::RhsElement { bind: None, elem: ast::RhsAst::Symbol(upper_terminal) }]),
                    //             ast::Action { expr: None }
                    //         )],
                    //         ty: None,
                    //         span: rs::DUMMY_SP,
                    //     });
                    // }
                        // TODO: check for available terminals.
                    ir.level = s["lexer_".len() ..].parse().unwrap();
                }
            }
            _ => {}
        }
    }
    // ir.level = stmts.level;
    Ok(ir)
}
