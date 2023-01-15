use std::{fmt::{Debug, Display}, hash::Hash, collections::{BTreeMap, BTreeSet}};

use elsa::{FrozenBTreeMap, FrozenVec};
use siphasher::sip128::{Hasher128, SipHasher24, Hash128};

use crate::values::*;

thread_local! {
    pub static PATHWAY_GRAPH: PathwayGraph = PathwayGraph::new();
}
pub struct FrozenPathwayGraph {
    nodes: FrozenVec<Box<Step>>,
    edges: FrozenBTreeMap<DirectedEdge, Box<()>>,
    nodes_by_hash: FrozenBTreeMap<NodeHash, Box<NodeRef>>,
    hashes_by_node: FrozenBTreeMap<NodeRef, Box<NodeHash>>,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<DirectedEdge>,
    nodes_by_hash: BTreeMap<NodeHash, NodeRef>,
    hashes_by_node: BTreeMap<NodeRef, NodeHash>,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NodeRef {
    id: NodeId,
    graph: GraphSlice,
}

#[derive(Copy, Clone, Debug)]
pub struct NodeHash {
    hash: Hash128,
}

impl Hash for NodeHash {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.hash.as_u128().hash(state);
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct GraphSlice {
    nodes: Epoch,
    edges: Epoch,
}

pub type NodeId = usize;
pub type Epoch = usize;
pub type Idx = usize;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Step {
    Group(GroupKind),
    Idx(Idx),
    Sequence { range: SequenceRange },
    SequenceEnd,
    SequenceToken,
    Rule { lhs: Symbol },
    Bind { label: Label },
    Stmt { lhs: Symbol },
    Symbol { symbol: Symbol },
    Type { ty: Type },
    GenerateSymbolForPath,
    External { node: NodeHash },
    Max,
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct SequenceRange {
    min: u32,
    max: Option<u32>,
}

#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
enum DirectedEdge {
    Forward(UndirectedEdge),
    Backward(UndirectedEdge),
}

#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
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

impl GraphSlice {
    // pub fn siblings(self, node_id: NodeId) -> impl Iterator<Item = NodeHash> {
    //     self.parents(node_id)
    //         .flat_map(move |parent| self.children(parent.id()))
    //         .filter(move |&child| child.id() != node_id)
    // }

    pub fn parents(self, child: NodeId) -> impl Iterator<Item = NodeHash> {
        let range = edge! { (child) <- (Epoch::MIN) <- (NodeId::MIN) }
            ..edge! { (child) <- (self.edges) <- (NodeId::MAX) };
        PATHWAY_GRAPH.with(|f|
            f
            .edges
            .map_range(range, |iter| iter.map(|(&edge, _val)| NodeRef::new(edge.backward_parent_id().unwrap(), self).hash()).collect::<Vec<_>>())
            .into_iter()
        )
    }

    pub fn outgoing_edges(self, parent: NodeId) -> impl Iterator<Item = UndirectedEdge> {
        let range = edge! { (parent) -> (Epoch::MIN) -> (NodeId::MIN) }
            ..edge! { (parent) -> (self.edges) -> (NodeId::MAX) };
        PATHWAY_GRAPH.with(|f|
            f.edges.map_range(range, |iter| iter.map(|(&edge, _val)| edge.undirected_edge()).collect::<Vec<_>>())
        )
        .into_iter()
    }

    pub fn children(self, parent: NodeId) -> impl Iterator<Item = NodeHash> {
        self.outgoing_edges(parent).map(move |undirected_edge| NodeRef::new(undirected_edge.b, self).hash())
    }

    pub fn iter(self) -> impl Iterator<Item = NodeHash> {
        (0..self.nodes).map(move |id| self.get_ref(id as NodeId).hash())
    }

    pub fn get_step(&self, node_id: NodeId) -> Option<Step> {
        if node_id < self.nodes as NodeId {
            PATHWAY_GRAPH.with(|g| g.nodes.get(node_id as usize).cloned())
        } else {
            None
        }
    }

    pub fn get_ref(self, node_id: NodeId) -> NodeRef {
        NodeRef {
            id: node_id,
            graph: self,
        }
    }

    pub fn roots(self) -> impl Iterator<Item = NodeHash> {
        let is_root = PATHWAY_GRAPH.with(|g|
            g.edges.map_keys(|edges| {
                let mut is_root = vec![true; self.nodes];
                for edge in edges {
                    is_root[edge.child_id() as usize] = false;
                }
                is_root
            })
        );
        self.iter()
            .zip(is_root.into_iter())
            .filter_map(|(node, is_root)| if is_root { Some(node) } else { None })
    }
}

impl NodeRef {
    pub fn new(id: NodeId, graph: GraphSlice) -> Self {
        NodeRef { id, graph }
    }

    pub fn step(self) -> Step {
        PATHWAY_GRAPH.with(|g| g.get_step(self.id).unwrap())
    }

    pub fn id(self) -> NodeId {
        self.id
    }

    pub fn add_edge_to<R: Into<NodeHash>>(self, end: R) {
        PathwayGraph::add_edge(self.id(), end.into())
    }

    pub fn add_edge_from<R: Into<NodeRef>>(self, start: R) {
        let start_node_ref = start.into();
        let start_id = start_node_ref.id();
        PathwayGraph::add_edge(start_id, self.hash())
    }

    pub fn children(self) -> impl Iterator<Item = NodeHash> {
        self.graph.children(self.id())
    }

    pub fn parents(self) -> impl Iterator<Item = NodeHash> {
        self.graph.parents(self.id())
    }

    pub fn outgoing_edges(self) -> impl Iterator<Item = UndirectedEdge> {
        self.graph.outgoing_edges(self.id())
    }

    /// memoized
    pub fn hash(self) -> NodeHash {
        if let Some(&result) = PATHWAY_GRAPH.with(|g| g.hashes_by_node.get(&self)) {
            result
        } else {
            let result = self.make_hash();
            PATHWAY_GRAPH.with(|g| {
                g.nodes_by_hash.insert(result, Box::new(self));
                g.hashes_by_node.insert(self, Box::new(result));
            });
            result
        }
    }

    /// not memoized
    pub fn make_hash(self) -> NodeHash {
        // TODO: too much hashing for big graphs?
        let mut hasher = SipHasher24::new();
        self.step().hash(&mut hasher);
        for edge in self.outgoing_edges() {
            let hash = NodeRef::new(edge.b, self.graph).hash();
            hash.hash(&mut hasher);
        }
        NodeHash { hash: hasher.finish128() }
    }
}

impl NodeHash {
    pub fn id(self) -> NodeId {
        PATHWAY_GRAPH.with(|f| f.nodes_by_hash.map_get(&self, |node| node.id()).expect("incorrect id"))
    }

    pub fn step(self) -> Step {
        PATHWAY_GRAPH.with(|f| f.nodes_by_hash.map_get(&self, |node| node.step()).expect("incorrect id"))
    }

    pub fn node_ref(self) -> NodeRef {
        PATHWAY_GRAPH.with(|f| f.nodes_by_hash.get(&self).cloned().expect("incorrect hash"))
    }

    pub fn children(self) -> impl Iterator<Item = NodeHash> {
        self.node_ref().children()
    }
    
    pub fn parents(self) -> impl Iterator<Item = NodeHash> {
        self.node_ref().parents()
    }
}

impl Into<NodeRef> for NodeHash {
    fn into(self) -> NodeRef {
        PATHWAY_GRAPH.with(|f| f.nodes_by_hash.get(&self).cloned().expect("incorrect hash"))
    }
}

impl Eq for NodeHash {}

impl PartialEq for NodeHash {
    fn eq(&self, other: &Self) -> bool {
        self.hash.as_u128().eq(&other.hash.as_u128())
    }
}

impl Ord for NodeHash {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.as_u128().cmp(&other.hash.as_u128())
    }
}

impl PartialOrd for NodeHash {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.hash.as_u128().partial_cmp(&other.hash.as_u128())
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

    fn undirected_edge(self) -> UndirectedEdge {
        match self {
            DirectedEdge::Forward(forward) => forward,
            DirectedEdge::Backward(backward) => backward,
        }
    }

    fn backward_parent_id(self) -> Option<NodeId> {
        match self {
            DirectedEdge::Backward(UndirectedEdge { a, epoch, b }) => Some(b),
            _ => None,
        }
    }
}

impl PathwayGraph {
    pub fn new() -> Self {
        PathwayGraph {
            nodes: FrozenVec::new(),
            edges: FrozenBTreeMap::new(),
            nodes_by_hash: FrozenBTreeMap::new(),
            hashes_by_node: FrozenBTreeMap::new(),
        }
    }

    // pub fn const_node(&self, step: Step, children: Vec<NodeHash>) -> NodeHash {
    //     let idx = self.nodes.len();
    //     self.nodes.push(Box::new(step));
    //     for child in children {
    //         self.add_edge(idx, child);
    //     }
    //     NodeRef::new(idx, self.slice()).hash()
    // }

    pub fn node(step: Step, children: Vec<NodeHash>) -> NodeHash {
        PATHWAY_GRAPH.with(|g| {
            let idx = g.nodes.len();
            g.nodes.push(Box::new(step));
            for child in children {
                PathwayGraph::add_edge(idx, child);
            }
            NodeRef::new(idx, g.slice()).hash()
        })
    }

    // pub fn add_edge(parent_id: NodeId, child: NodeHash) {
    //     PATHWAY_GRAPH.with(|g| {
    //         let child_node_ref: NodeRef = child.into();
    //         let child_id = child_node_ref.id(); 
    //         g.edges
    //             .insert(edge! { (parent_id) -> (g.current_epoch()) -> (child_id) }, Box::new(()));
    //         g.edges
    //             .insert(edge! { (child_id) <- (g.current_epoch()) <- (parent_id) }, Box::new(()));
    //     })
    // }

    pub fn add_edge(parent_id: NodeId, child: NodeHash) {
        PATHWAY_GRAPH.with(|g| {
            let child_node_ref: NodeRef = child.into();
            let child_id = child_node_ref.id(); 
            g.edges
                .insert(edge! { (parent_id) -> (g.current_epoch()) -> (child_id) }, Box::new(()));
            g.edges
                .insert(edge! { (child_id) <- (g.current_epoch()) <- (parent_id) }, Box::new(()));
        })
    }

    // pub fn siblings(&self, node_id: NodeId) -> impl Iterator<Item = NodeHash> + '_ {
    //     self.parents(node_id)
    //         .flat_map(move |parent| self.children(parent.id()))
    //         .filter(move |&child| child.id != node_id)
    // }

    pub fn parents(&self, child: NodeId) -> impl Iterator<Item = NodeHash> {
        self.slice().parents(child)
    }

    pub fn children(&self, parent: NodeId) -> impl Iterator<Item = NodeHash> {
        self.slice().children(parent)
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeHash> {
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

    pub fn roots(&self) -> impl Iterator<Item = NodeHash> {
        self.slice().roots()
    }
}
