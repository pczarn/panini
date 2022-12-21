use std::collections::BTreeSet;

#[derive(Clone, Debug)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<Edge>,
}

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

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Edge {
    Forward { a: usize, b: usize },
    Backward { b: usize, a: usize },
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum IdxKind {
    Sum,
    Product,
    Stmt,
}

pub type IdentId = u32;
pub type ExprId = u32;
pub type AttrId = u32;
pub type LexerId = u32;
pub type TyId = u32;
pub type BindId = u32;
pub type FragmentId = u32;
pub type SpanId = u32;
pub type NodeId = usize;

pub const NULL_NODE: usize = 0;

impl PathwayGraph {
    pub fn new() -> Self {
        // let null_node = Node::Step { step: Step::Max, child: NULL_NODE, parent: NULL_NODE };
        // PathwayGraph { nodes: vec![null_node] }
        PathwayGraph {
            nodes: vec![],
            edges: BTreeSet::new(),
        }
    }

    pub fn node(&mut self, step: Step, children: Vec<NodeId>) -> NodeId {
        let idx = self.nodes.len();
        self.nodes.push(step);
        for child in children {
            self.add_edge(idx, child);
        }
        idx
    }

    fn add_edge(&mut self, parent: NodeId, child: NodeId) {
        self.edges.insert(Edge::Forward {
            a: parent,
            b: child,
        });
        self.edges.insert(Edge::Backward {
            b: child,
            a: parent,
        });
    }

    pub fn siblings(&self, node: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        self.parents(node)
            .flat_map(move |parent| self.children(parent))
            .filter(move |&child| child != node)
    }

    pub fn parents(&self, child: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        let parent_edge_a = Edge::Backward { b: child, a: 0 };
        let parent_edge_b = Edge::Backward { b: child, a: !0 };
        self.edges.range(parent_edge_a..parent_edge_b).map(|&edge| {
            if let Edge::Backward { a, .. } = edge {
                a
            } else {
                unreachable!()
            }
        })
    }

    pub fn children(&self, parent: NodeId) -> impl Iterator<Item = NodeId> + '_ {
        let sibling_edge_a = Edge::Forward { a: parent, b: 0 };
        let sibling_edge_b = Edge::Forward { a: parent, b: !0 };
        self.edges
            .range(sibling_edge_a..sibling_edge_b)
            .map(|&edge| {
                if let Edge::Forward { b, .. } = edge {
                    b
                } else {
                    unreachable!()
                }
            })
    }

    pub fn iter(&self) -> impl Iterator<Item = &Step> + '_ {
        self.nodes.iter()
    }

    pub fn get(&self, step_id: NodeId) -> Option<Step> {
        self.nodes.get(step_id).cloned()
    }

    pub fn roots(&self) -> impl Iterator<Item = NodeId> {
        let mut is_root = vec![true; self.nodes.len()];
        for &edge in &self.edges {
            match edge {
                Edge::Forward { a, b } => {
                    is_root[b as usize] = false;
                }
                _ => {}
            }
        }
        is_root
            .into_iter()
            .enumerate()
            .filter_map(|(id, root)| if root { Some(id) } else { None })
    }

    pub fn walk<'a>(&'a self) -> impl Iterator<Item = NodeWithChildren<'a>> {
        self.iter().enumerate().map(|(i, &node)| NodeWithChildren {
            id: i as NodeId,
            children: Box::new(self.children(i as NodeId)) as Box<dyn Iterator<Item = NodeId> + 'a>,
        })
    }
}

struct NodeWithChildren<'a> {
    id: NodeId,
    children: Box<dyn Iterator<Item = NodeId> + 'a>,
}
