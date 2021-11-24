use std::collections::BTreeSet;

use input::{BindId, FragmentId, TyId};
// use input::{Parameters, AttrId, BindId, ExprId, FragmentId, LexerId, SpanId, TyId};

pub const NULL_NODE: usize = 0;

pub type StepId = usize;

#[derive(Clone)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<Edge>,
}

macro_rules! pathway_graph {
    ($($step:expr),*) => (
        
    )
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Edge {
    Forward {
        a: usize,
        b: usize,
    },
    Backward {
        b: usize,
        a: usize,
    },
}

// #[derive(Clone, Debug, Hash, Eq, PartialEq)]
// pub enum Node {
//     Step {
//         step: Step,
//         child: usize,
//         parent: usize,
//     },
//     Branch {
//         children: Vec<usize>,
//         parent: usize,
//     },
// }

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Idx(IdxKind, usize),
    Fragment(FragmentId),
    StmtFragment(FragmentId),
    StmtTy(TyId),
    // Class(Class, Symbol),
    // RightQuote,
    Bind { bind_id: BindId, idx: usize },
    Sequence { min: u32, max: Option<u32> },
    SequenceEnd,
    SequenceToken,
    Max,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum IdxKind {
    Sum,
    Product,
    Stmt,
}

impl PathwayGraph {
    pub fn new() -> Self {
        // let null_node = Node::Step { step: Step::Max, child: NULL_NODE, parent: NULL_NODE };
        // PathwayGraph { nodes: vec![null_node] }
        PathwayGraph {
            nodes: vec![],
            edges: BTreeSet::new(),
        }
    }

    pub fn node(&mut self, step: Step, children: Vec<StepId>) -> StepId {
        let idx = self.nodes.len();
        self.nodes.push(step);
        for child in children {
            self.add_edge(idx, child);
        }
        idx
    }

    fn add_edge(&mut self, parent: StepId, child: StepId) {
        self.edges.insert(Edge::Forward {
            a: parent,
            b: child,
        });
        self.edges.insert(Edge::Backward {
            b: child,
            a: parent,
        });
    }

    pub fn siblings(&self, node: StepId) -> impl Iterator<Item=StepId> + '_ {
        self.parents(node)
            .flat_map(move |parent| self.children(parent))
            .filter(move |&child| child != node)
    }

    pub fn parents(&self, child: StepId) -> impl Iterator<Item=StepId> + '_ {
        let parent_edge_a = Edge::Backward { b: child, a: 0 };
        let parent_edge_b = Edge::Backward { b: child, a: !0 };
        self.edges.range(parent_edge_a .. parent_edge_b)
            .map(|&edge| {
                if let Edge::Backward { a, .. } = edge { a } else { unreachable!() }
            })
    }

    pub fn children(&self, parent: StepId) -> impl Iterator<Item=StepId> + '_ {
        let sibling_edge_a = Edge::Forward { a: parent, b: 0 };
        let sibling_edge_b = Edge::Forward { a: parent, b: !0 };
        self.edges.range(sibling_edge_a .. sibling_edge_b)
            .map(|&edge| {
                if let Edge::Forward { b, .. } = edge { b } else { unreachable!() }
            })
    }

    pub fn iter(&self) -> impl Iterator<Item=&Step> + '_ {
        self.nodes.iter()
    }

    // pub fn branch_node(&mut self, children: Vec<usize>) -> usize {
    //     let idx = self.nodes.len();
    //     for &i in &children {
    //         *self.nodes[i].parent_mut() = idx;
    //     }
    //     self.nodes.push(Node::Branch { children, parent: NULL_NODE });
    //     idx
    // }
}

// impl Node {
//     fn parent_mut(&mut self) -> &mut usize {
//         match self {
//             &mut Node::Step { ref mut parent, .. } => {
//                 parent
//             }
//             &mut Node::Branch { ref mut parent, .. } => {
//                 parent
//             }
//         }
//     }
// }
