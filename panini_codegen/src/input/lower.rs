use panini_logic::input as logic_input;
use panini_logic::input::ast as logic_ast;
use input::ast;

pub struct LowerAst {
    tables: Tables,
}

impl LowerAst {
    pub fn new() -> Self {
        LowerAst {
            tables: Tables::new(),
        }
    }

    pub fn rewrite_stmts(&mut self, input_tree: ast::InputTree) -> logic_ast::InputTree {
        logic_ast::InputTree {
            attr_arguments: logic_ast::AttrArguments {
                lexer_arguments: 
            },
            pathways: input_tree.pathways.into_iter().map(|pathway|
                self.rewrite_pathway(pathway)
            ).collect(),
            lexer: 
        }
    }

    pub fn rewrite_pathway(&mut self, pathway: ast::Pathway) -> logic_ast::Pathway {
        logic_ast::Pathway {
            steps: pathway.steps.into_iter().map(|step| self.rewrite_step(step)).collect(),
        }
    }

    pub fn rewrite_step(&mut self, step: ast::Step) -> logic_ast::Step {
        match step {
            ast::Step::Alternative(num) => logic_ast::Step::Alternative(num),
            ast::Step::Idx(num) => logic_ast::Step::Idx(num),
            ast::Step::StmtIdx(num) => logic_ast::Step::StmtIdx(num),
            ast::Step::Sequence { min, max } => logic_ast::Step::Sequence { min, max },

            ast::Step::SequenceEnd => unreachable!(),
            ast::Step::SequenceToken => unreachable!(),
            ast::Step::Max => unreachable!(),

            ast::Step::Fragment(term) => {
                logic_ast::Step::Fragment(self.tables.intern_fragment(term))
            }
            ast::Step::StmtFragment(term) => {
                logic_ast::Step::StmtFragment(self.tables.intern_fragment(term))
            }
            ast::Step::StmtTy(ty) => {
                logic_ast::Step::StmtTy(self.tables.intern_ty(ty))
            }
            // Class(Class, Symbol),
            // RightQuote,
            ast::Step::Bind { bind_id, idx } => {
                logic_ast::Step::Bind { idx, bind_id: self.tables.intern_bind(bind_id) }
            }
        }
    }
}
