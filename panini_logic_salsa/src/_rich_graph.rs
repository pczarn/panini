use std::collections::BTreeMap;

use elsa::FrozenIndexSet;
use indexmap::IndexSet;

use crate::graph::{PathwayGraph, FragmentId, NodeId, BindId};

#[derive(Clone)]
pub struct RichGraph {
    graph: PathwayGraph,
    sym_set: IndexSet<String>,
    bind_set: IndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

pub struct FrozenRichGraph {
    graph: PathwayGraph,
    sym_set: FrozenIndexSet<String>,
    bind_set: FrozenIndexSet<String>,
    rule_indices: BTreeMap<String, usize>,
}

#[derive(Debug)]
pub struct RichGraphDebug<'a> {
    graph: &'a PathwayGraph,
    sym_set: Vec<String>,
    bind_set: Vec<String>,
    rule_indices: &'a BTreeMap<String, usize>,
}

impl RichGraph {
    pub fn sym(&self, fragment_id: FragmentId) -> Option<&str> {
        self.sym_set.iter().nth(fragment_id as usize).map(|s| &s[..])
    }

    pub fn bind(&self, bind_id: BindId) -> Option<&str> {
        self.bind_set.iter().nth(bind_id as usize).map(|s| &s[..])
    }

    pub fn graph(&self) -> &PathwayGraph {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut PathwayGraph {
        &mut self.graph
    }
}

impl ::std::fmt::Debug for RichGraph {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        fn interner_to_vec(set: &IndexSet<String>) -> Vec<String> {
            let mut result = vec![];
            for i in 0usize.. {
                if let Some(elem) = set.get_index(i) {
                    result.push(elem.to_string());
                } else {
                    break;
                }
            }
            result
        }
        RichGraphDebug {
            graph: &self.graph,
            sym_set: interner_to_vec(&self.sym_set),
            bind_set: interner_to_vec(&self.bind_set),
            rule_indices: &self.rule_indices,
        }
        .fmt(fmt)
    }
}

impl FrozenRichGraph {
    pub fn new() -> Self {
        FrozenRichGraph {
            graph: PathwayGraph::new(),
            sym_set: FrozenIndexSet::new(),
            bind_set: FrozenIndexSet::new(),
            rule_indices: BTreeMap::new(),
        }
    }

    pub fn intern_fragment(&mut self, ident: &str) -> FragmentId {
        self.sym_set.insert_full(ident.to_string()).0 as FragmentId
    }

    pub fn intern_bind(&mut self, ident: &str) -> FragmentId {
        self.bind_set.insert_full(ident.to_string()).0 as FragmentId
    }

    pub fn next_stmt_idx(&mut self, lhs_str: &str) -> usize {
        let entry = self.rule_indices.entry(lhs_str.to_string()).or_insert(0);
        let result = *entry;
        *entry += 1;
        result
    }

    pub fn thaw(self) -> RichGraph {
        RichGraph {
            graph: self.graph,
            sym_set: self.sym_set.into_set(),
            bind_set: self.bind_set.into_set(),
            rule_indices: self.rule_indices,
        }
    }

    pub fn graph_mut(&mut self) -> &mut PathwayGraph {
        &mut self.graph
    }
}
