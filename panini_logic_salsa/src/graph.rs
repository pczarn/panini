use std::fmt::{Debug, Display};

use elsa::{FrozenBTreeMap, FrozenVec};

thread_local! {
    static PATHWAY_GRAPH: PathwayGraph = PathwayGraph::new();
}

pub struct PathwayGraph {
    nodes: FrozenVec<Step>,
    edges: FrozenBTreeMap<DirectedEdge, Epoch>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NodeRef {
    id: NodeId,
    graph: GraphSlice,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Step {
    Group(GroupKind),
    Idx(Idx),
    Sequence { min: u32, max: Option<u32> },
    SequenceEnd,
    SequenceToken,
    Rule,
    Bind { label: Box<dyn Label> },
    Stmt { lhs: Box<dyn Symbol> },
    Symbol { symbol: Box<dyn Symbol> },
    Type { ty: Box<dyn Type> },
    GenerateSymbolForPath,
    Max,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum DirectedEdge {
    Forward(UndirectedEdge),
    Backward(UndirectedEdge),
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct UndirectedEdge {
    a: NodeId,
    epoch: Epoch,
    b: NodeId,
}

macro_rules! edge {
    (($a:expr) -- ($epoch:expr) -- ($b:expr)) => {
        UndirectedEdge {
            a: $a,
            epoch: $epoch,
            b: $b,
        }
    };
    (($a:expr) -> ($epoch:expr) -> ($b:expr)) => {
        DirectedEdge::Forward(edge! { ($a) -- ($epoch) -- ($b) })
    };
    (($a:expr) <- ($epoch:expr) <- ($b:expr)) => {
        DirectedEdge::Backward(edge! { ($a) -- ($epoch) -- ($b) })
    };
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum GroupKind {
    Sum,
    Product,
    Stmt,
}

pub struct GraphSlice {
    nodes: Epoch,
    edges: Epoch,
}

pub trait Symbol: Debug + Display {}
pub trait Label: Debug + Display {}
pub trait Type: Debug + Display {}

pub type NodeId = usize;
pub type Epoch = usize;
pub type Idx = usize;

pub const NULL_NODE: NodeId = 0;

impl GraphSlice {
    pub fn siblings(self, node_id: NodeId) -> impl Iterator<Item = NodeRef> {
        self.parents(node_id)
            .flat_map(move |parent| self.children(parent))
            .filter(move |&child| child.id != node_id)
    }

    pub fn parents(self, child: NodeId) -> impl Iterator<Item = NodeRef> {
        let range = edge! { (child) <- (Epoch::MIN) <- (NodeId::MIN) }
            ..edge! { (child) <- (self.edges) <- (NodeId::MAX) };
        PATHWAY_GRAPH
            .edges
            .range(range)
            .map(|&edge| edge.backward_parent())
    }

    pub fn children(self, parent: NodeId) -> impl Iterator<Item = NodeRef> {
        let range = edge! { (parent) -> (Epoch::MIN) -> (NodeId::MIN) }
            ..edge! { (parent) -> (self.edges) -> (NodeId::MAX) };
        PATHWAY_GRAPH
            .edges
            .range(range)
            .map(|&edge| edge.forward_child())
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeRef> + '_ {
        (0..self.nodes).map(|id| self.get_ref(id as NodeId))
    }

    pub fn get_step(&self, node_id: NodeId) -> Option<Step> {
        if node_id < self.nodes as NodeId {
            return None;
        }
        PATHWAY_GRAPH.nodes.get(node_id as usize).cloned()
    }

    pub fn get_ref(self, node_id: NodeId) -> NodeRef {
        NodeRef {
            id: node_id,
            graph: self,
        }
    }

    pub fn roots(&self) -> impl Iterator<Item = NodeRef> {
        let mut is_root = vec![true; self.nodes.len()];
        for &edge in self.edges.keys() {
            is_root[edge.child_id() as usize] = false;
        }
        self.iter()
            .zip(is_root.into_iter())
            .filter_map(|(node, is_root)| if is_root { Some(node) } else { None })
    }
}

impl NodeRef {
    pub fn step(self) -> Step {
        PATHWAY_GRAPH.get(self.id)
    }

    pub fn id(self) -> NodeId {
        self.id
    }

    pub fn add_edge_to(self, end: NodeRef) {
        PATHWAY_GRAPH.add_edge(self, end)
    }

    pub fn add_edge_from(self, start: NodeRef) {
        PATHWAY_GRAPH.add_edge(start, self)
    }

    pub fn children(self) -> impl Iterator<Item = NodeRef> {
        PATHWAY_GRAPH.children(self)
    }
}

impl DirectedEdge {
    fn parent_id(self) -> NodeId {
        match self {
            DirectedEdge::Forward(UndirectedEdge { a, epoch, b }) => a,
            DirectedEdge::Backward(UndirectedEdge { a, epoch, b }) => b,
        }
    }

    fn child_id(self) -> NodeId {
        match self {
            DirectedEdge::Forward(UndirectedEdge { a, epoch, b }) => b,
            DirectedEdge::Backward(UndirectedEdge { a, epoch, b }) => a,
        }
    }
}

impl PathwayGraph {
    pub fn new() -> Self {
        PathwayGraph {
            nodes: FrozenVec::new(),
            edges: FrozenBTreeMap::new(),
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
        self.edges
            .insert(edge! { (parent) -> (self.current_epoch()) -> (child) });
        self.edges
            .insert(edge! { (child) <- (self.current_epoch()) <- (parent) });
    }

    pub fn siblings(&self, node_id: NodeId) -> impl Iterator<Item = NodeRef> + '_ {
        self.parents(node_id)
            .flat_map(move |parent| self.children(parent))
            .filter(move |&child| child.id != node_id)
    }

    pub fn parents(&self, child: NodeId) -> impl Iterator<Item = NodeRef> {
        self.slice().parents(child)
    }

    pub fn children(&self, parent: NodeId) -> impl Iterator<Item = NodeRef> {
        self.slice().children(parent)
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeRef> {
        self.slice().iter()
    }

    pub fn slice(&self) -> GraphSlice {
        GraphSlice {
            nodes: self.nodes.len(),
            edges: self.current_epoch(),
        }
    }

    pub fn current_epoch(&self) -> Epoch {
        self.edges.len()
    }

    pub fn get_step(&self, node_id: NodeId) -> Option<Step> {
        self.nodes.get(node_id as usize).cloned()
    }

    pub fn get_ref(&self, node_id: NodeId) -> NodeRef {
        NodeRef {
            id: node_id,
            graph: self.slice(),
        }
    }

    pub fn roots(&self) -> impl Iterator<Item = NodeRef> {
        let mut is_root = vec![true; self.nodes.len()];
        for &edge in self.edges.keys() {
            is_root[edge.child_id() as usize] = false;
        }
        self.iter()
            .zip(is_root.into_iter())
            .filter_map(|(node, is_root)| if is_root { Some(node) } else { None })
    }
}
