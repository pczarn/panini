use std::collections::BTreeSet;

#[derive(Clone, Debug)]
pub struct PathwayGraph {
    nodes: Vec<Step>,
    edges: BTreeSet<Edge>,
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