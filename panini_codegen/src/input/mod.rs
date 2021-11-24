use middle::Tables;
use panini_logic::input::ast as logic_ast;

pub mod lower;

pub struct Input {
    pub pathways: PathwayGraph,
    pub tables: Tables,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Idx(logic_ast::IdxKind, usize),
    Fragment(rs::Term),
    StmtFragment(rs::Term),
    StmtTy(rs::TokenStream),
    Bind { bind_id: BindId, idx: usize },
    Sequence {
        min: u32,
        max: Option<u32>,
    },
    SequenceEnd,
    SequenceToken,
    Max,
}

impl Input {
    pub fn new() -> Self {
        Input {
            pathway_graph: PathwayGraph::new(),
            tables: Tables::new(),
        }
    }

    pub fn node(&mut self, step: Step, children: Vec<usize>) -> usize {
        self.pathway_graph.node(self.lower_step(step), children)
    }

    fn add_edge(&mut self, parent: usize, child: usize) {
        self.pathway_graph.add_edge(parent, child)
    }

    fn lower_step(&mut self, step: Step) -> logic_ast::Step {
        match step {
            Step::Idx(kind, num) => logic_ast::Step::Idx(num),
            Step::Sequence { min, max } => logic_ast::Step::Sequence { min, max },

            Step::SequenceEnd => unreachable!(),
            Step::SequenceToken => unreachable!(),
            Step::Max => unreachable!(),

            Step::Fragment(term) => {
                logic_ast::Step::Fragment(self.tables.intern_fragment(term))
            }
            Step::StmtFragment(term) => {
                logic_ast::Step::StmtFragment(self.tables.intern_fragment(term))
            }
            Step::StmtTy(ty) => {
                logic_ast::Step::StmtTy(self.tables.intern_ty(ty))
            }
            Step::Bind { bind_id, idx } => {
                logic_ast::Step::Bind { idx, bind_id: self.tables.intern_bind(bind_id) }
            }
        }
    }
}
